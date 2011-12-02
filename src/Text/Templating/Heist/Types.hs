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

module Text.Templating.Heist.Types where

------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Arrow
import             Control.Monad.CatchIO
import             Control.Monad.Cont
import             Control.Monad.Error
import             Control.Monad.Reader
import             Control.Monad.State
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
-- | A Splice is a HeistT computation that returns a 'Template'.
type Splice m = HeistT m Template


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map Text (Splice m)


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @HeistState@ using any of Heist's @HeistState m -> HeistState m@
-- \"filter\" functions.  Then you use the resulting @HeistState@ in calls to
-- @renderTemplate@.
data HeistState m = HeistState {
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
    -- | Temporary flag for backwards compatibility with the old attribute
    -- syntax for splices.
    , _oldAttributeSyntax :: Bool
}


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
templateNames :: HeistState m -> [TPath]
templateNames ts = Map.keys $ _templateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the splices defined in a HeistState.
spliceNames :: HeistState m -> [Text]
spliceNames ts = Map.keys $ _spliceMap ts


------------------------------------------------------------------------------
instance (Monad m) => Monoid (HeistState m) where
    mempty = HeistState Map.empty Map.empty True [] 0
                           return return return [] Nothing False

    (HeistState s1 t1 r1 _ d1 o1 b1 a1 dt1 ctf1 oas1) `mappend`
        (HeistState s2 t2 r2 c2 d2 o2 b2 a2 dt2 ctf2 oas2) =
        HeistState s t r c2 d (o1 >=> o2) (b1 >=> b2) (a1 >=> a2)
            (dt1 `mappend` dt2) ctf (oas1 || oas2)
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2
        r = r1 && r2
        d = max d1 d2
        ctf = getLast $ Last ctf1 `mappend` Last ctf2


------------------------------------------------------------------------------
instance Eq (HeistState m) where
    a == b = (_recurse a == _recurse b) &&
             (_templateMap a == _templateMap b) &&
             (_curContext a == _curContext b)


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateStateTyCon :: TyCon
templateStateTyCon = mkTyCon "Text.Templating.Heist.HeistState"
{-# NOINLINE templateStateTyCon #-}

instance (Typeable1 m) => Typeable (HeistState m) where
    typeOf _ = mkTyConApp templateStateTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- | HeistT is the monad used for 'Splice' processing.  HeistT provides
-- \"passthrough\" instances for many of the monads you might use in the inner
-- monad.
newtype HeistT m a = HeistT {
    runHeistT :: X.Node
              -> HeistState m
              -> m (a, HeistState m)
}


------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalHeistT :: Monad m
           => HeistT m a
           -> X.Node
           -> HeistState m
           -> m a
evalHeistT m r s = do
    (a, _) <- runHeistT m r s
    return a


------------------------------------------------------------------------------
-- | Functor instance
instance Functor m => Functor (HeistT m) where
    fmap f (HeistT m) = HeistT $ \r s -> first f <$> m r s


------------------------------------------------------------------------------
-- | Applicative instance
instance (Monad m, Functor m) => Applicative (HeistT m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
-- | Monad instance
instance Monad m => Monad (HeistT m) where
    return a = HeistT (\_ s -> return (a, s))
    HeistT m >>= k = HeistT $ \r s -> do
        (a, s') <- m r s
        runHeistT (k a) r s'


------------------------------------------------------------------------------
-- | MonadIO instance
instance MonadIO m => MonadIO (HeistT m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
-- | MonadTrans instance
instance MonadTrans HeistT where
    lift m = HeistT $ \_ s -> do
        a <- m
        return (a, s)


------------------------------------------------------------------------------
-- | MonadCatchIO instance
instance MonadCatchIO m => MonadCatchIO (HeistT m) where
    catch (HeistT a) h = HeistT $ \r s -> do
       let handler e = runHeistT (h e) r s
       catch (a r s) handler
    block (HeistT m) = HeistT $ \r s -> block (m r s)
    unblock (HeistT m) = HeistT $ \r s -> unblock (m r s)


------------------------------------------------------------------------------
-- | MonadFix passthrough instance
instance MonadFix m => MonadFix (HeistT m) where
    mfix f = HeistT $ \r s ->
        mfix $ \ (a, _) -> runHeistT (f a) r s


------------------------------------------------------------------------------
-- | Alternative passthrough instance
instance (Functor m, MonadPlus m) => Alternative (HeistT m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
-- | MonadPlus passthrough instance
instance MonadPlus m => MonadPlus (HeistT m) where
    mzero = lift mzero
    m `mplus` n = HeistT $ \r s ->
        runHeistT m r s `mplus` runHeistT n r s


------------------------------------------------------------------------------
-- | MonadState passthrough instance
instance MonadState s m => MonadState s (HeistT m) where
    get = lift get
    put = lift . put


------------------------------------------------------------------------------
-- | MonadReader passthrough instance
instance MonadReader r m => MonadReader r (HeistT m) where
    ask = HeistT $ \_ s -> do
            r <- ask
            return (r,s)
    local f (HeistT m) =
        HeistT $ \r s -> local f (m r s)


------------------------------------------------------------------------------
-- | Helper for MonadError instance.
liftCatch :: (m (a,HeistState m)
              -> (e -> m (a,HeistState m))
              -> m (a,HeistState m))
          -> HeistT m a
          -> (e -> HeistT m a)
          -> HeistT m a
liftCatch ce m h =
    HeistT $ \r s ->
        (runHeistT m r s `ce`
        (\e -> runHeistT (h e) r s))


------------------------------------------------------------------------------
-- | MonadError passthrough instance
instance (MonadError e m) => MonadError e (HeistT m) where
    throwError = lift . throwError
    catchError = liftCatch catchError


------------------------------------------------------------------------------
-- | Helper for MonadCont instance.
liftCallCC :: ((((a,HeistState m) -> m (b, HeistState m))
                  -> m (a, HeistState m))
                -> m (a, HeistState m))
           -> ((a -> HeistT m b) -> HeistT m a)
           -> HeistT m a
liftCallCC ccc f = HeistT $ \r s ->
    ccc $ \c ->
    runHeistT (f (\a -> HeistT $ \_ _ -> c (a, s))) r s


------------------------------------------------------------------------------
-- | MonadCont passthrough instance
instance (MonadCont m) => MonadCont (HeistT m) where
    callCC = liftCallCC callCC


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Text.Templating.Heist.HeistT"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (HeistT m) where
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
getParamNode :: Monad m => HeistT m X.Node
getParamNode = HeistT $ \r s -> return (r,s)


------------------------------------------------------------------------------
-- | HeistT's 'local'.
localParamNode :: Monad m
               => (X.Node -> X.Node)
               -> HeistT m a
               -> HeistT m a
localParamNode f m = HeistT $ \r s -> runHeistT m (f r) s


------------------------------------------------------------------------------
-- | HeistT's 'gets'.
getsTS :: Monad m => (HeistState m -> r) -> HeistT m r
getsTS f = HeistT $ \_ s -> return (f s, s)


------------------------------------------------------------------------------
-- | HeistT's 'get'.
getTS :: Monad m => HeistT m (HeistState m)
getTS = HeistT $ \_ s -> return (s, s)


------------------------------------------------------------------------------
-- | HeistT's 'put'.
putTS :: Monad m => HeistState m -> HeistT m ()
putTS s = HeistT $ \_ _ -> return ((), s)


------------------------------------------------------------------------------
-- | HeistT's 'modify'.
modifyTS :: Monad m
                    => (HeistState m -> HeistState m)
                    -> HeistT m ()
modifyTS f = HeistT $ \_ s -> return ((), f s)


------------------------------------------------------------------------------
-- | Restores the HeistState.  This function is almost like putTS except it
-- preserves the current doctypes.  You should use this function instead of
-- @putTS@ to restore an old state.  This was needed because doctypes needs to
-- be in a "global scope" as opposed to the template call "local scope" of
-- state items such as recursionDepth, curContext, and spliceMap.
restoreTS :: Monad m => HeistState m -> HeistT m ()
restoreTS old = modifyTS (\cur -> old { _doctypes = _doctypes cur })


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a HeistT computation with
-- a modified heist state.
localTS :: Monad m
        => (HeistState m -> HeistState m)
        -> HeistT m a
        -> HeistT m a
localTS f k = do
    ts <- getTS
    putTS $ f ts
    res <- k
    restoreTS ts
    return res

