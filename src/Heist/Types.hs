{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module contains the core Heist data types.  HeistT intentionally
does not expose any of its functionality via MonadState or MonadReader
functions.  We define passthrough instances for the most common types of
monads.  These instances allow the user to use HeistT in a monad stack
without needing calls to `lift`.

Edward Kmett wrote most of the HeistT code and associated instances,
liberating us from the unused writer portion of RWST.

-}

module Heist.Types where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.CatchIO
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.ByteString.Char8 (ByteString)
import           Data.DList                      (DList)
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import           Data.HeterogeneousEnvironment   (HeterogeneousEnvironment)
import qualified Data.HeterogeneousEnvironment as HE
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Typeable
import           Prelude hiding (catch)
import qualified Text.XmlHtml as X

import Debug.Trace

tr s x = trace (s++show x) x

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
newtype RuntimeSplice m a = RuntimeSplice {
      unRT :: StateT HeterogeneousEnvironment m a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadState HeterogeneousEnvironment
               , MonadTrans )


------------------------------------------------------------------------------
instance (Monad m, Monoid a) => Monoid (RuntimeSplice m a) where
    mempty = return mempty

    a `mappend` b = do
        !x <- a
        !y <- b
        return $! x `mappend` y


------------------------------------------------------------------------------
data Chunk m = Pure !Builder
               -- ^ output known at load time
             | RuntimeHtml !(RuntimeSplice m Builder)
               -- ^ output computed at run time
             | RuntimeAction !(RuntimeSplice m ())
               -- ^ runtime action used only for its side-effect


------------------------------------------------------------------------------
instance Show (Chunk m) where
    show (Pure a)          = T.unpack $ T.concat
        ["Pure \"", decodeUtf8 $ toByteString a, "\""]
    show (RuntimeHtml _)   = "RuntimeHtml <m>"
    show (RuntimeAction _) = "RuntimeAction <m>"


type CompiledSplice m = HeistT m IO (DList (Chunk m))

------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @HeistState@ using any of Heist's @HeistState -> HeistState@
-- \"filter\" functions.  Then you use the resulting @HeistState@ in calls to
-- @renderTemplate@.
--
-- n is the runtime monad
-- m is the load time monad
data HeistState n m = HeistState {
    -- | A mapping of splice names to splice actions
      _spliceMap        :: HashMap Text (HeistT n n Template)
    -- | A mapping of template names to templates
    , _templateMap      :: HashMap TPath DocumentFile

    -- | A mapping of splice names to splice actions
    , _compiledSpliceMap   :: HashMap Text (CompiledSplice n)
    -- | A mapping of template names to templates
    , _compiledTemplateMap :: HashMap TPath (n Builder)

    -- | A flag to control splice recursion
    , _recurse          :: Bool
    -- | The path to the template currently being processed.
    , _curContext       :: TPath
    -- | A counter keeping track of the current recursion depth to prevent
    -- infinite loops.
    , _recursionDepth   :: Int
    -- | A hook run on all templates at load time.
    , _onLoadHook       :: Template -> IO Template
    -- | The doctypes encountered during template processing.
    , _doctypes         :: [X.DocType]
    -- | The full path to the current template's file on disk.
    , _curTemplateFile  :: Maybe FilePath
    -- | A key generator used to produce new unique Promises.
    , _keygen           :: HE.KeyGen

    , _failHardAndFast  :: Bool
}


-- NOTE: We got rid of the Monoid instance because it is absolutely not safe
-- to combine two compiledTemplateMaps.  All compiled templates must be known
-- at load time and processed in a single call to initHeist/loadTemplates or
-- whatever we end up calling it..

instance (Typeable1 m) => Typeable (HeistState n m) where
    typeOf _ = mkTyConApp templateStateTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- | HeistT is the monad transformer used for 'Splice' processing.  HeistT
-- provides \"passthrough\" instances for many of the monads you might use in
-- the inner monad.
--
-- n is the runtime monad (the first parameter passed to HeistState)
-- m is the monad being run now
newtype HeistT n m a = HeistT {
    runHeistT :: X.Node
              -> HeistState n m
              -> m (a, HeistState n m)
}


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
templateNames :: HeistState n m -> [TPath]
templateNames ts = H.keys $ _templateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
compiledTemplateNames :: HeistState n m -> [TPath]
compiledTemplateNames ts = H.keys $ _compiledTemplateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the splices defined in a HeistState.
spliceNames :: HeistState n m -> [Text]
spliceNames ts = H.keys $ _spliceMap ts


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateStateTyCon :: TyCon
templateStateTyCon = mkTyCon "Heist.HeistState"
{-# NOINLINE templateStateTyCon #-}

------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalHeistT :: (Monad m)
           => HeistT n m a
           -> X.Node
           -> HeistState n m
           -> m a
evalHeistT m r s = do
    (a, _) <- runHeistT m r s
    return a
{-# INLINE evalHeistT #-}


------------------------------------------------------------------------------
-- | Functor instance
instance Functor m => Functor (HeistT n m) where
    fmap f (HeistT m) = HeistT $ \r s -> first f <$> m r s


------------------------------------------------------------------------------
-- | Applicative instance
instance (Monad m, Functor m) => Applicative (HeistT n m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
-- | Monad instance
instance Monad m => Monad (HeistT n m) where
    return a = HeistT (\_ s -> return (a, s))
    {-# INLINE return #-}
    HeistT m >>= k = HeistT $ \r s -> do
        (a, s') <- m r s
        runHeistT (k a) r s'
    {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
-- | MonadIO instance
instance MonadIO m => MonadIO (HeistT n m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
-- | MonadTrans instance
instance MonadTrans (HeistT n) where
    lift m = HeistT $ \_ s -> do
        a <- m
        return (a, s)


------------------------------------------------------------------------------
-- | MonadCatchIO instance
instance MonadCatchIO m => MonadCatchIO (HeistT n m) where
    catch (HeistT a) h = HeistT $ \r s -> do
       let handler e = runHeistT (h e) r s
       catch (a r s) handler
    block (HeistT m) = HeistT $ \r s -> block (m r s)
    unblock (HeistT m) = HeistT $ \r s -> unblock (m r s)


------------------------------------------------------------------------------
-- | MonadFix passthrough instance
instance MonadFix m => MonadFix (HeistT n m) where
    mfix f = HeistT $ \r s ->
        mfix $ \ (a, _) -> runHeistT (f a) r s


------------------------------------------------------------------------------
-- | Alternative passthrough instance
instance (Functor m, MonadPlus m) => Alternative (HeistT n m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
-- | MonadPlus passthrough instance
instance MonadPlus m => MonadPlus (HeistT n m) where
    mzero = lift mzero
    m `mplus` n = HeistT $ \r s ->
        runHeistT m r s `mplus` runHeistT n r s


------------------------------------------------------------------------------
-- | MonadState passthrough instance
instance MonadState s m => MonadState s (HeistT n m) where
    get = lift get
    {-# INLINE get #-}
    put = lift . put
    {-# INLINE put #-}


------------------------------------------------------------------------------
-- | MonadReader passthrough instance
instance MonadReader r m => MonadReader r (HeistT n m) where
    ask = HeistT $ \_ s -> do
            r <- ask
            return (r,s)
    local f (HeistT m) =
        HeistT $ \r s -> local f (m r s)


------------------------------------------------------------------------------
-- | Helper for MonadError instance.
liftCatch :: (m (a,HeistState n m)
              -> (e -> m (a,HeistState n m))
              -> m (a,HeistState n m))
          -> HeistT n m a
          -> (e -> HeistT n m a)
          -> HeistT n m a
liftCatch ce m h =
    HeistT $ \r s ->
        (runHeistT m r s `ce`
        (\e -> runHeistT (h e) r s))


------------------------------------------------------------------------------
-- | MonadError passthrough instance
instance (MonadError e m) => MonadError e (HeistT n m) where
    throwError = lift . throwError
    catchError = liftCatch catchError


------------------------------------------------------------------------------
-- | Helper for MonadCont instance.
liftCallCC :: ((((a,HeistState n m) -> m (b, HeistState n m))
                  -> m (a, HeistState n m))
                -> m (a, HeistState n m))
           -> ((a -> HeistT n m b) -> HeistT n m a)
           -> HeistT n m a
liftCallCC ccc f = HeistT $ \r s ->
    ccc $ \c ->
    runHeistT (f (\a -> HeistT $ \_ _ -> c (a, s))) r s


------------------------------------------------------------------------------
-- | MonadCont passthrough instance
instance (MonadCont m) => MonadCont (HeistT n m) where
    callCC = liftCallCC callCC


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Heist.HeistT"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (HeistT n m) where
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
getParamNode :: Monad m => HeistT n m X.Node
getParamNode = HeistT $ \r s -> return (r,s)
{-# INLINE getParamNode #-}


------------------------------------------------------------------------------
-- | HeistT's 'local'.
localParamNode :: Monad m
               => (X.Node -> X.Node)
               -> HeistT n m a
               -> HeistT n m a
localParamNode f m = HeistT $ \r s -> runHeistT m (f r) s
{-# INLINE localParamNode #-}


------------------------------------------------------------------------------
-- | HeistT's 'gets'.
getsTS :: Monad m => (HeistState n m -> r) -> HeistT n m r
getsTS f = HeistT $ \_ s -> return (f s, s)
{-# INLINE getsTS #-}


------------------------------------------------------------------------------
-- | HeistT's 'get'.
getTS :: Monad m => HeistT n m (HeistState n m)
getTS = HeistT $ \_ s -> return (s, s)
{-# INLINE getTS #-}


------------------------------------------------------------------------------
-- | HeistT's 'put'.
putTS :: Monad m => HeistState n m -> HeistT n m ()
putTS s = HeistT $ \_ _ -> return ((), s)
{-# INLINE putTS #-}


------------------------------------------------------------------------------
-- | HeistT's 'modify'.
modifyTS :: Monad m
         => (HeistState n m -> HeistState n m)
         -> HeistT n m ()
modifyTS f = HeistT $ \_ s -> return ((), f s)
{-# INLINE modifyTS #-}


------------------------------------------------------------------------------
-- | Restores the HeistState.  This function is almost like putTS except it
-- preserves the current doctypes.  You should use this function instead of
-- @putTS@ to restore an old state.  This was needed because doctypes needs to
-- be in a "global scope" as opposed to the template call "local scope" of
-- state items such as recursionDepth, curContext, and spliceMap.
restoreTS :: Monad m => HeistState n m -> HeistT n m ()
restoreTS old = modifyTS (\cur -> old { _doctypes = _doctypes cur })
{-# INLINE restoreTS #-}


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a HeistT computation with
-- a modified heist state.
localTS :: Monad m
        => (HeistState n m -> HeistState n m)
        -> HeistT n m a
        -> HeistT n m a
localTS f k = do
    ts <- getTS
    putTS $ f ts
    res <- k
    restoreTS ts
    return res
{-# INLINE localTS #-}


------------------------------------------------------------------------------
-- | Modifies the recursion depth.
modRecursionDepth :: Monad m => (Int -> Int) -> HeistT n m ()
modRecursionDepth f =
    modifyTS (\st -> st { _recursionDepth = f (_recursionDepth st) })


------------------------------------------------------------------------------
-- | AST to hold attribute parsing structure.  This is necessary because
-- attoparsec doesn't support parsers running in another monad.
data AttAST = Literal Text
            | Ident   Text
  deriving (Show)


------------------------------------------------------------------------------
isLiteral :: AttAST -> Bool
isLiteral (Literal _) = True
isLiteral _           = False


