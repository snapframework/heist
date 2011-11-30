{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- | A Splice is a TemplateMonad computation that returns a 'Template'.
type Splice m = TemplateMonad m Template


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


{-# DEPRECATED TemplateState "NOTICE: The name TemplateState is changing to HeistState.  Use HeistState instead of TemplateState." #-}
------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @HeistState@ using any of Heist's @HeistState m -> HeistState m@
-- \"filter\" functions.  Then you use the resulting @HeistState@ in calls to
-- @renderTemplate@.
type TemplateState = HeistState


{-# DEPRECATED useOldAttributeSyntax "NOTICE: This function is only here temporarily to ease the transition in attribute syntax.  It will be removed in the next major release.  Update your templates!" #-}
------------------------------------------------------------------------------
-- | Sets compatibility mode that uses the old $() syntax for splices in
-- attributes.  The old syntax conflicts with the ubiquitous jquery function.
-- The new syntax is ${}.  This compatibility mode will be removed in the next
-- major release.
--
-- See https://github.com/snapframework/heist/issues/12 for the discussion.
useOldAttributeSyntax :: HeistState m -> HeistState m
useOldAttributeSyntax ts = ts { _oldAttributeSyntax = True }


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


{-# DEPRECATED TemplateMonad "NOTICE: The name TemplateMonad is being phased out in favor of the more appropriate HeistT.  Change your code now to prevent breakage in the future!" #-}
------------------------------------------------------------------------------
-- | TemplateMonad is the monad used for 'Splice' processing.  TemplateMonad
-- provides \"passthrough\" instances for many of the monads you might use in
-- the inner monad.
newtype TemplateMonad m a = TemplateMonad {
    runTemplateMonad :: X.Node
                     -> HeistState m
                     -> m (a, HeistState m)
}
type HeistT = TemplateMonad


------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalTemplateMonad :: Monad m
                  => TemplateMonad m a
                  -> X.Node
                  -> HeistState m
                  -> m a
evalTemplateMonad m r s = do
    (a, _) <- runTemplateMonad m r s
    return a


------------------------------------------------------------------------------
-- | Functor instance
instance Functor m => Functor (TemplateMonad m) where
    fmap f (TemplateMonad m) = TemplateMonad $ \r s -> first f <$> m r s


------------------------------------------------------------------------------
-- | Applicative instance
instance (Monad m, Functor m) => Applicative (TemplateMonad m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
-- | Monad instance
instance Monad m => Monad (TemplateMonad m) where
    return a = TemplateMonad (\_ s -> return (a, s))
    TemplateMonad m >>= k = TemplateMonad $ \r s -> do
        (a, s') <- m r s
        runTemplateMonad (k a) r s'


------------------------------------------------------------------------------
-- | MonadIO instance
instance MonadIO m => MonadIO (TemplateMonad m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
-- | MonadTrans instance
instance MonadTrans TemplateMonad where
    lift m = TemplateMonad $ \_ s -> do
        a <- m
        return (a, s)


------------------------------------------------------------------------------
-- | MonadCatchIO instance
instance MonadCatchIO m => MonadCatchIO (TemplateMonad m) where
    catch (TemplateMonad a) h = TemplateMonad $ \r s -> do
       let handler e = runTemplateMonad (h e) r s
       catch (a r s) handler
    block (TemplateMonad m) = TemplateMonad $ \r s -> block (m r s)
    unblock (TemplateMonad m) = TemplateMonad $ \r s -> unblock (m r s)


------------------------------------------------------------------------------
-- | MonadFix passthrough instance
instance MonadFix m => MonadFix (TemplateMonad m) where
    mfix f = TemplateMonad $ \r s ->
        mfix $ \ (a, _) -> runTemplateMonad (f a) r s


------------------------------------------------------------------------------
-- | Alternative passthrough instance
instance (Functor m, MonadPlus m) => Alternative (TemplateMonad m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
-- | MonadPlus passthrough instance
instance MonadPlus m => MonadPlus (TemplateMonad m) where
    mzero = lift mzero
    m `mplus` n = TemplateMonad $ \r s ->
        runTemplateMonad m r s `mplus` runTemplateMonad n r s


------------------------------------------------------------------------------
-- | MonadState passthrough instance
instance MonadState s m => MonadState s (TemplateMonad m) where
    get = lift get
    put = lift . put


------------------------------------------------------------------------------
-- | MonadReader passthrough instance
instance MonadReader r m => MonadReader r (TemplateMonad m) where
    ask = TemplateMonad $ \_ s -> do
            r <- ask
            return (r,s)
    local f (TemplateMonad m) =
        TemplateMonad $ \r s -> local f (m r s)


------------------------------------------------------------------------------
-- | Helper for MonadError instance.
liftCatch :: (m (a,HeistState m)
              -> (e -> m (a,HeistState m))
              -> m (a,HeistState m))
          -> TemplateMonad m a
          -> (e -> TemplateMonad m a)
          -> TemplateMonad m a
liftCatch ce m h =
    TemplateMonad $ \r s ->
        (runTemplateMonad m r s `ce`
        (\e -> runTemplateMonad (h e) r s))


------------------------------------------------------------------------------
-- | MonadError passthrough instance
instance (MonadError e m) => MonadError e (TemplateMonad m) where
    throwError = lift . throwError
    catchError = liftCatch catchError


------------------------------------------------------------------------------
-- | Helper for MonadCont instance.
liftCallCC :: ((((a,HeistState m) -> m (b, HeistState m))
                  -> m (a, HeistState m))
                -> m (a, HeistState m))
           -> ((a -> TemplateMonad m b) -> TemplateMonad m a)
           -> TemplateMonad m a
liftCallCC ccc f = TemplateMonad $ \r s ->
    ccc $ \c ->
    runTemplateMonad (f (\a -> TemplateMonad $ \_ _ -> c (a, s))) r s


------------------------------------------------------------------------------
-- | MonadCont passthrough instance
instance (MonadCont m) => MonadCont (TemplateMonad m) where
    callCC = liftCallCC callCC


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Text.Templating.Heist.TemplateMonad"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (TemplateMonad m) where
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
getParamNode :: Monad m => TemplateMonad m X.Node
getParamNode = TemplateMonad $ \r s -> return (r,s)


------------------------------------------------------------------------------
-- | TemplateMonad's 'local'.
localParamNode :: Monad m
               => (X.Node -> X.Node)
               -> TemplateMonad m a
               -> TemplateMonad m a
localParamNode f m = TemplateMonad $ \r s -> runTemplateMonad m (f r) s


------------------------------------------------------------------------------
-- | TemplateMonad's 'gets'.
getsTS :: Monad m => (HeistState m -> r) -> TemplateMonad m r
getsTS f = TemplateMonad $ \_ s -> return (f s, s)


------------------------------------------------------------------------------
-- | TemplateMonad's 'get'.
getTS :: Monad m => TemplateMonad m (HeistState m)
getTS = TemplateMonad $ \_ s -> return (s, s)


------------------------------------------------------------------------------
-- | TemplateMonad's 'put'.
putTS :: Monad m => HeistState m -> TemplateMonad m ()
putTS s = TemplateMonad $ \_ _ -> return ((), s)


------------------------------------------------------------------------------
-- | TemplateMonad's 'modify'.
modifyTS :: Monad m
                    => (HeistState m -> HeistState m)
                    -> TemplateMonad m ()
modifyTS f = TemplateMonad $ \_ s -> return ((), f s)


------------------------------------------------------------------------------
-- | Restores the HeistState.  This function is almost like putTS except it
-- preserves the current doctypes.  You should use this function instead of
-- @putTS@ to restore an old state.  This was needed because doctypes needs to
-- be in a "global scope" as opposed to the template call "local scope" of
-- state items such as recursionDepth, curContext, and spliceMap.
restoreTS :: Monad m => HeistState m -> TemplateMonad m ()
restoreTS old = modifyTS (\cur -> old { _doctypes = _doctypes cur })


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a TemplateMonad computation with
-- a modified heist state.
localTS :: Monad m
        => (HeistState m -> HeistState m)
        -> TemplateMonad m a
        -> TemplateMonad m a
localTS f k = do
    ts <- getTS
    putTS $ f ts
    res <- k
    restoreTS ts
    return res

