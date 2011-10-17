{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-|

This module contains the core Heist data types.  TemplateMonad intentionally
does not expose any of its functionality via MonadState or MonadReader
functions.  We define passthrough instances for the most common types of
monads.  These instances allow the user to use TemplateMonad in a monad stack
without needing calls to `lift`.

Edward Kmett wrote most of the TemplateMonad code and associated instances,
liberating us from the unused writer portion of RWST.

-}

module Text.Templating.Heist.Types where

------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Arrow
import             Control.Monad.Cont
import             Control.Monad.Error
import             Control.Monad.IO.Control
import             Control.Monad.Reader
import             Control.Monad.State
import             Control.Monad.Trans.Control
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.Map as Map
import             Data.Map (Map)
import             Data.Monoid
import             Data.Text (Text)
import             Data.Typeable
import             Prelude hiding (catch)
import qualified   Text.XmlHtml as X


------------------------------------------------------------------------------
-- | A 'Template' is a forest of XML nodes.  Here we deviate from the "single
-- root node" constraint of well-formed XML because we want to allow templates
-- to contain fragments of a document that may not have a single root.
type Template = [X.Node]


------------------------------------------------------------------------------
-- | MIME Type.  The type alias is here to make the API clearer.
type MIMEType = ByteString


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
-- currently being processed.
type TPath = [ByteString]


data DocumentFile = DocumentFile
    { dfDoc  :: X.Document
    , dfFile :: Maybe FilePath
    } deriving (Eq)

------------------------------------------------------------------------------
-- | All documents representing templates are stored in a map.
type TemplateMap = Map TPath DocumentFile


------------------------------------------------------------------------------
-- | A Splice is a TemplateMonad computation that returns a 'Template'.
type Splice m = TemplateMonad m Template


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map Text (Splice m)


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @TemplateState@ using any of Heist's @TemplateState m ->
-- TemplateState m@ \"filter\" functions.  Then you use the resulting
-- @TemplateState@ in calls to @renderTemplate@.
data TemplateState m = TemplateState {
    -- | A mapping of splice names to splice actions
      _spliceMap       :: SpliceMap m
    -- | A mapping of template names to templates
    , _templateMap     :: TemplateMap
    -- | A flag to control splice recursion
    , _recurse         :: Bool
    -- | The path to the template currently being processed.
    , _curContext      :: TPath
    -- | A counter keeping track of the current recursion depth to prevent
    -- infinite loops.
    , _recursionDepth  :: Int
    -- | A hook run on all templates at load time.
    , _onLoadHook      :: Template -> IO Template
    -- | A hook run on all templates just before they are rendered.
    , _preRunHook      :: Template -> m Template
    -- | A hook run on all templates just after they are rendered.
    , _postRunHook     :: Template -> m Template
    -- | The doctypes encountered during template processing.
    , _doctypes        :: [X.DocType]
    -- | The full path to the current template's file on disk.
    , _curTemplateFile :: Maybe FilePath
}


------------------------------------------------------------------------------
instance (Monad m) => Monoid (TemplateState m) where
    mempty = TemplateState Map.empty Map.empty True [] 0
                           return return return [] Nothing

    (TemplateState s1 t1 r1 _ d1 o1 b1 a1 dt1 ctf1) `mappend`
        (TemplateState s2 t2 r2 c2 d2 o2 b2 a2 dt2 ctf2) =
        TemplateState s t r c2 d (o1 >=> o2) (b1 >=> b2) (a1 >=> a2)
            (dt1 `mappend` dt2) ctf
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2
        r = r1 && r2
        d = max d1 d2
        ctf = getLast $ Last ctf1 `mappend` Last ctf2


------------------------------------------------------------------------------
instance Eq (TemplateState m) where
    a == b = (_recurse a == _recurse b) &&
             (_templateMap a == _templateMap b) &&
             (_curContext a == _curContext b)


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateStateTyCon :: TyCon
templateStateTyCon = mkTyCon "Text.Templating.Heist.TemplateState"
{-# NOINLINE templateStateTyCon #-}

instance (Typeable1 m) => Typeable (TemplateState m) where
    typeOf _ = mkTyConApp templateStateTyCon [typeOf1 (undefined :: m ())]


{-# DEPRECATED TemplateMonad "NOTICE: The name TemplateMonad is being phased out in favor of the more appropriate HeistT.  Change your code now to prevent breakage in the future!" #-}
------------------------------------------------------------------------------
-- | TemplateMonad is the monad used for 'Splice' processing.  TemplateMonad
-- provides \"passthrough\" instances for many of the monads you might use in
-- the inner monad.
{- newtype TemplateMonad m a = TemplateMonad { -}
{-     runTemplateMonad :: X.Node-}
{-                      -> TemplateState m-}
{-                      -> m (a, TemplateState m)-}
{- } -}
type TemplateMonad m a = TemplateHeistT m a
type TemplateHeistT m a = HeistT (TemplateState m) m a

newtype HeistT ts m a = HeistT {
    runTemplateMonad :: X.Node
                     -> ts
                     -> m (a, ts)
}


------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalTemplateMonad :: Monad m
                  => HeistT (TemplateState m) m a
                  -> X.Node
                  -> TemplateState m
                  -> m a
evalTemplateMonad m r s = do
    (a, _) <- runTemplateMonad m r s
    return a


------------------------------------------------------------------------------
-- | Functor instance
instance Functor m => Functor (HeistT ts m) where
    fmap f (HeistT m) = HeistT $ \r s -> first f <$> m r s


------------------------------------------------------------------------------
-- | Applicative instance
instance (Monad m, Functor m) => Applicative (HeistT ts m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
-- | Monad instance
instance Monad m => Monad (HeistT ts m) where
    return a = HeistT (\_ s -> return (a, s))
    HeistT m >>= k = HeistT $ \r s -> do
        (a, s') <- m r s
        runTemplateMonad (k a) r s'


------------------------------------------------------------------------------
-- | MonadIO instance
instance MonadIO m => MonadIO (HeistT ts m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
-- | MonadTrans instance
instance MonadTrans (HeistT ts) where
    lift m = HeistT $ \_ s -> do
        a <- m
        return (a, s)


------------------------------------------------------------------------------
-- | MonadControlIO instance
instance MonadControlIO m => MonadControlIO (HeistT ts m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadTransControl (HeistT ts) where
    liftControl = liftControl'
      where
        liftControl' :: Monad m => (Run (HeistT ts) -> m a) -> HeistT ts m a
        liftControl' f =
          HeistT $ \xn ts ->
              let run :: forall n o b. (Monad n, Monad o, Monad (HeistT ts o))
                      => HeistT ts n b -> n (HeistT ts o b)
                  run t = liftM (\(nd, ts') -> HeistT
                                     $ \_ _ -> return (nd, ts'))
                                (runTemplateMonad t xn ts)
              in  liftM (\x -> (x, ts)) (f run)

------------------------------------------------------------------------------
-- | MonadFix passthrough instance
instance MonadFix m => MonadFix (HeistT ts m) where
    mfix f = HeistT $ \r s ->
        mfix $ \ (a, _) -> runTemplateMonad (f a) r s


------------------------------------------------------------------------------
-- | Alternative passthrough instance
instance (Functor m, MonadPlus m) => Alternative (HeistT ts m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
-- | MonadPlus passthrough instance
instance MonadPlus m => MonadPlus (HeistT ts m) where
    mzero = lift mzero
    m `mplus` n = HeistT $ \r s ->
        runTemplateMonad m r s `mplus` runTemplateMonad n r s


------------------------------------------------------------------------------
-- | MonadState passthrough instance
instance MonadState s m => MonadState s (HeistT ts m) where
    get = lift get
    put = lift . put


------------------------------------------------------------------------------
-- | MonadReader passthrough instance
instance MonadReader r m => MonadReader r (HeistT (TemplateState m) m) where
    ask = HeistT $ \_ s -> do
            r <- ask
            return (r,s)
    local f (HeistT m) =
        HeistT $ \r s -> local f (m r s)


------------------------------------------------------------------------------
-- | Helper for MonadError instance.
liftCatch :: (m (a,TemplateState m)
              -> (e -> m (a,TemplateState m))
              -> m (a,TemplateState m))
          -> HeistT (TemplateState m) m a
          -> (e -> HeistT (TemplateState m) m a)
          -> HeistT (TemplateState m) m a
liftCatch ce m h =
    HeistT $ \r s ->
        (runTemplateMonad m r s `ce`
        (\e -> runTemplateMonad (h e) r s))


------------------------------------------------------------------------------
-- | MonadError passthrough instance
instance (MonadError e m) => MonadError e (HeistT (TemplateState m) m) where
    throwError = lift . throwError
    catchError = liftCatch catchError


------------------------------------------------------------------------------
-- | Helper for MonadCont instance.
liftCallCC :: ((((a,TemplateState m) -> m (b, TemplateState m))
                  -> m (a, TemplateState m))
                -> m (a, TemplateState m))
           -> ((a -> HeistT (TemplateState m) m b) -> HeistT (TemplateState m) m a)
           -> HeistT (TemplateState m) m a
liftCallCC ccc f = HeistT $ \r s ->
    ccc $ \c ->
    runTemplateMonad (f (\a -> HeistT $ \_ _ -> c (a, s))) r s


------------------------------------------------------------------------------
-- | MonadCont passthrough instance
instance (MonadCont m) => MonadCont (HeistT (TemplateState m) m) where
    callCC = liftCallCC callCC


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Text.Templating.Heist.HeistT"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (HeistT ts m) where
    typeOf1 _ = mkTyConApp templateMonadTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- Functions for our monad.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Gets the node currently being processed.
--
--   > <speech author="Shakespeare">
--   >   To sleep, perchance to dream.
--   > </speech>
--
-- When you call @getParamNode@ inside the code for the @speech@ splice, it
-- returns the Node for the @speech@ tag and its children.  @getParamNode >>=
-- childNodes@ returns a list containing one 'TextNode' containing part of
-- Hamlet's speech.  @liftM (getAttribute \"author\") getParamNode@ would
-- return @Just "Shakespeare"@.
getParamNode :: Monad m => HeistT ts m X.Node
getParamNode = HeistT $ \r s -> return (r,s)


------------------------------------------------------------------------------
-- | HeistT's 'local'.
localParamNode :: Monad m
               => (X.Node -> X.Node)
               -> HeistT ts m a
               -> HeistT ts m a
localParamNode f m = HeistT $ \r s -> runTemplateMonad m (f r) s


------------------------------------------------------------------------------
-- | HeistT's 'gets'.
getsTS :: Monad m => (TemplateState m -> r) -> HeistT (TemplateState m) m r
getsTS f = HeistT $ \_ s -> return (f s, s)


------------------------------------------------------------------------------
-- | HeistT's 'get'.
getTS :: Monad m => HeistT (TemplateState m) m (TemplateState m)
getTS = HeistT $ \_ s -> return (s, s)


------------------------------------------------------------------------------
-- | HeistT's 'put'.
putTS :: Monad m => TemplateState m -> HeistT (TemplateState m) m ()
putTS s = HeistT $ \_ _ -> return ((), s)


------------------------------------------------------------------------------
-- | HeistT's 'modify'.
modifyTS :: Monad m
                    => (TemplateState m -> TemplateState m)
                    -> HeistT (TemplateState m) m ()
modifyTS f = HeistT $ \_ s -> return ((), f s)


------------------------------------------------------------------------------
-- | Restores the TemplateState.  This function is almost like putTS except it
-- preserves the current doctypes.  You should use this function instead of
-- @putTS@ to restore an old state.  This was needed because doctypes needs to
-- be in a "global scope" as opposed to the template call "local scope" of
-- state items such as recursionDepth, curContext, and spliceMap.
restoreTS :: Monad m => TemplateState m -> HeistT (TemplateState m) m ()
restoreTS old = modifyTS (\cur -> old { _doctypes = _doctypes cur })


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a HeistT computation with
-- a modified template state.
localTS :: Monad m
        => (TemplateState m -> TemplateState m)
        -> HeistT (TemplateState m) m a
        -> HeistT (TemplateState m) m a
localTS f k = do
    ts <- getTS
    putTS $ f ts
    res <- k
    restoreTS ts
    return res

