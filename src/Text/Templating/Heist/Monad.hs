{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports the 'HeistT' monad transformer and the related
'MonadHeist' type class. 'MonadHeist' instances are defined any 'Monad'
transformed by 'HeistT', as well as passthroughs for any 'MonadHeist' wrapped
around one of the common monad transformers.

-}

module Text.Templating.Heist.Monad
    ( MonadHeist(..)
    , HeistT
    , runHeistT'
    , runHeistT
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.List
import           Control.Monad.RWS.Strict hiding (pass)
import qualified Control.Monad.RWS.Lazy as LRWS
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Control.Monad.State.Lazy as LState
import           Control.Monad.Writer.Strict hiding (pass)
import qualified Control.Monad.Writer.Lazy as LWriter
import           Control.Monad.Trans
import           Data.ByteString(ByteString)
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


------------------------------------------------------------------------------
-- | The 'MonadHeist' typeclass. A 'MonadHeist' is kind of like a 'Reader'
-- with a 'TemplateState' as its environment, but it also supports the
-- additional operations of rendering and reloading.
class (Monad m, Monad n) => MonadHeist m n | n -> m where
    -- | Retrieves the 'TemplateState'.
    heistState  :: n (TemplateState m)

    -- | Runs an action with a modified 'TemplateState'. You might want to use
    -- this if you had a set of splices which were customised for a specific
    -- action. To do that you would do:
    --
    -- > heistLocal (bindSplices mySplices)
    heistLocal  :: (TemplateState m -> TemplateState m) -> n a -> n a

    -- | Attempts to retrieve the template with the given name from the
    -- environment and render it with the splices currently in the
    -- environment. 'Nothing' is returned if the template with that name was
    -- not found.
    heistRender :: ByteString -> n (Maybe ByteString)

    -- | Attempts to clear the 'StaticTagState' and reload the templates from
    -- disk. If it fails to do that, an error message is returned in the
    -- 'Left' of the 'Either'.
    heistReload :: n (Either String ())


------------------------------------------------------------------------------
-- | HeistState is the internal state of any 'MonadHeist'. 'HeistT' is just a
-- 'ReaderT' with a 'HeistT' as its environment. It stores the 'TemplateState'
-- and the 'StaticTagState' and enough additional information to support the
-- 'heistReload' operation which flushes the 'StaticTagState' and reloads all
-- the templates in the 'TemplateState' from disk.
data HeistState m = HeistState
    { _path     :: FilePath
    , _origTs   :: TemplateState m
    , _tsMVar   :: MVar (TemplateState m)
    , _sts      :: StaticTagState
    , _modifier :: TemplateState m -> TemplateState m
    }


------------------------------------------------------------------------------
-- | The 'HeistT' monad transformer.
--
-- This is defined with a simple type alias instead of the more common idiom
-- of using a @newtype@ combined with the @GeneralizedNewtypeDeriving@
-- extension to automatically derive all the required instances. The problem
-- with that it required the programmer to specify every monad typeclass which
-- needs to be derived in the @deriving@ clause, but in this case the
-- programmer can't know which monad typeclasses are required. If a user makes
-- a @Foo@ monad, with an associated @MonadFoo@ typeclass with passthrough
-- instances for all the common transformers, then @HeistT Foo@ wouldn't be a
-- @MonadFoo@, because @HeistT@ is not likely to have a passthrough instance
-- for @MonadFoo@ (it certainly wouldn't if it was defined in a library which
-- knew nothing about Heist). However, if we use a simple type alias, then
-- 'HeistT' is really just a 'ReaderT', which /does/ have passthrough
-- instances for most monad typeclasses in the wild.
--
-- However, it is requested that users do not rely on the 'MonadReader'
-- instance provided by 'HeistT', as this is simply an implementation detail
-- that may change at any time.
type HeistT m a = ReaderT (HeistState m) m a


------------------------------------------------------------------------------
-- | Runs a 'HeistT' computation. It returns an 'Either', representing failure
-- (e.g., the given directory did not exist) with a 'Left' and otherwise
-- returning a 'Right'. It is in the 'IO' monad and not 'm' because if loading
-- the templates is going to fail, it is desirable to have that happen outside
-- of 'm', and so we only put a value in 'm' on success.
runHeistT' :: MonadIO m
           => HeistT m a
           -> FilePath
           -- ^ The 'HeistT' computation.
           -> TemplateState m 
           -- ^ The directory containing the templates. 
           -> IO (Either String (m a))
           -- ^ The initial template state (might be 'emptyTemplateState' with
           -- the addition of some global splices).
runHeistT' m path origTs = do
    (staticTs,sts) <- bindStaticTag origTs
    ets <- loadTemplates path staticTs
    leftPass ets $ \ts -> do
        tsMVar <- newMVar ts
        return $ runReaderT m $ HeistState path origTs tsMVar sts id


------------------------------------------------------------------------------
-- | A variant of 'runHeistT'' which calls 'fail' directly if it encounters
-- an error loading the templates, as opposed to wrapping the error in an
-- 'Either'.
runHeistT :: MonadIO m
           => HeistT m a
           -> FilePath
           -- ^ The 'HeistT' computation.
           -> TemplateState m 
           -- ^ The directory containing the templates. 
           -> IO (m a)
           -- ^ The initial template state (might be 'emptyTemplateState' with
           -- the addition of some global splices).
runHeistT m path origTs = either fail return =<< runHeistT' m path origTs


------------------------------------------------------------------------------
instance MonadIO m => MonadHeist m (ReaderT (HeistState m) m) where
    heistState = do
        (HeistState _ _ tsMVar _ modifier) <- ask
        return . modifier =<< (liftIO $ readMVar tsMVar)

    heistLocal f = local $ \s -> s { _modifier = f . (_modifier s) }

    heistRender s = heistState >>= \m -> lift $ renderTemplate m s

    heistReload = do
        (HeistState path origTs tsMVar sts _) <- ask
        liftIO $ do
            clearStaticTagCache $ sts
            ets <- loadTemplates path origTs
            leftPass ets $ modifyMVar_ tsMVar . const . return


------------------------------------------------------------------------------
-- | A utility function that prepends an error onto a Left.
leftPass :: Monad m => Either String b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError) (liftM Right . m) e
  where
    loadError = (++) "Error loading templates: "


------------------------------------------------------------------------------
instance MonadHeist m n => MonadHeist m (ContT c n) where
    heistState     = lift heistState
    heistLocal f m = ContT $ \c -> heistLocal f $ runContT m c
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance (MonadHeist m n, Error e) => MonadHeist m (ErrorT e n) where
    heistState     = lift heistState
    heistLocal f m = ErrorT $ heistLocal f $ runErrorT m
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance MonadHeist m n => MonadHeist m (ListT n) where
    heistState     = lift heistState
    heistLocal f m = ListT $ heistLocal f $ runListT m
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance (MonadHeist m n, Monoid w) => MonadHeist m (RWST r w s n) where
    heistState     = lift heistState
    heistLocal f m = RWST $ \r s -> heistLocal f $ runRWST m r s
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance (MonadHeist m n, Monoid w) => MonadHeist m (LRWS.RWST r w s n) where
    heistState     = lift heistState
    heistLocal f m = LRWS.RWST $ \r s -> heistLocal f $ LRWS.runRWST m r s
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance MonadHeist m n => MonadHeist m (ReaderT r n) where
    heistState     = lift heistState
    heistLocal f m = ReaderT $ \r -> heistLocal f $ runReaderT m r
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance MonadHeist m n => MonadHeist m (StateT s n) where
    heistState     = lift heistState
    heistLocal f m = StateT $ \s -> heistLocal f $ runStateT m s
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance MonadHeist m n => MonadHeist m (LState.StateT s n) where
    heistState     = lift heistState
    heistLocal f m = LState.StateT $ \s -> heistLocal f $ LState.runStateT m s
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance (MonadHeist m n, Monoid w) => MonadHeist m (WriterT w n) where
    heistState     = lift heistState
    heistLocal f m = WriterT $ heistLocal f $ runWriterT m
    heistRender    = lift . heistRender
    heistReload    = lift heistReload


------------------------------------------------------------------------------
instance (MonadHeist m n, Monoid w) => MonadHeist m (LWriter.WriterT w n) where
    heistState     = lift heistState
    heistLocal f m = LWriter.WriterT $ heistLocal f $ LWriter.runWriterT m
    heistRender    = lift . heistRender
    heistReload    = lift heistReload
