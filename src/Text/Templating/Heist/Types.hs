{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

This module contains the core Heist data types.  TemplateMonad intentionally
does not expose any of it's functionality via MonadState or MonadReader
functions.  We define passthrough instances for the most common types of
monads.  These instances allow the user to use TemplateMonad in a monad stack
without needing calls to `lift`.

Edward Kmett wrote most of the TemplateMonad code and associated instances,
liberating us from the unused writer portion of RWST.

-}

module Text.Templating.Heist.Types where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid
import           Data.Typeable
import           Prelude hiding (catch)
import qualified Text.XML.Expat.Tree as X


------------------------------------------------------------------------------
-- | Heist templates are XML documents. The hexpat library is polymorphic over
-- the type of strings, so here we define a 'Node' alias to fix the string
-- types of the tag names and tag bodies to 'ByteString'.
type Node = X.Node ByteString ByteString


------------------------------------------------------------------------------
-- | A 'Template' is a forest of XML nodes.  Here we deviate from the "single
-- root node" constraint of well-formed XML because we want to allow templates
-- to contain fragments of a document that may not have a single root.
type Template = [Node]


------------------------------------------------------------------------------
-- | An 'InternalTemplate' carries a doctype with it that we get from the
-- template at load time.  The tricks that we're playing so templates don't
-- have to have a single root node screw up doctypes, so we have to handle
-- them manually.
data InternalTemplate = InternalTemplate {
    _itDoctype :: Maybe ByteString,
    _itNodes   :: [Node]
} deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
-- currently being processed.
type TPath = [ByteString]


------------------------------------------------------------------------------
-- | All templates are stored in a map.
type TemplateMap = Map TPath InternalTemplate


------------------------------------------------------------------------------
-- | A Splice is a TemplateMonad computation that returns a 'Template'.
type Splice m = TemplateMonad m Template


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map ByteString (Splice m)


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @TemplateState@ using any of Heist's @TemplateState m ->
-- TemplateState m@ \"filter\" functions.  Then you use the resulting
-- @TemplateState@ in calls to @renderTemplate@.
data TemplateState m = TemplateState {
    -- | A mapping of splice names to splice actions
      _spliceMap      :: SpliceMap m
    -- | A mapping of template names to templates
    , _templateMap    :: TemplateMap
    -- | A flag to control splice recursion
    , _recurse        :: Bool
    -- | The path to the template currently being processed.
    , _curContext     :: TPath
    -- | A counter keeping track of the current recursion depth to prevent
    -- infinite loops.
    , _recursionDepth :: Int
    -- | A hook run on all templates at load time.
    , _onLoadHook     :: Template -> IO Template
    -- | A hook run on all templates just before they are rendered.
    , _preRunHook     :: Template -> m Template
    -- | A hook run on all templates just after they are rendered.
    , _postRunHook    :: Template -> m Template
    -- | The doctypes encountered during template processing.
    , _doctypes       :: [ByteString]
}


------------------------------------------------------------------------------
instance (Monad m) => Monoid (TemplateState m) where
    mempty = TemplateState Map.empty Map.empty True [] 0
                           return return return []

    (TemplateState s1 t1 r1 _ d1 o1 b1 a1 dt1) `mappend`
        (TemplateState s2 t2 r2 c2 d2 o2 b2 a2 dt2) =
        TemplateState s t r c2 d (o1 >=> o2) (b1 >=> b2) (a1 >=> a2)
            (dt1 `mappend` dt2)
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2
        r = r1 && r2
        d = max d1 d2


------------------------------------------------------------------------------
instance Eq (TemplateState m) where
    a == b = (_recurse a == _recurse b) &&
             (_templateMap a == _templateMap b) &&
             (_curContext a == _curContext b)


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateStateTyCon :: TyCon
templateStateTyCon = mkTyCon "Text.Templating.Heist.Types.TemplateState"
{-# NOINLINE templateStateTyCon #-}

instance (Typeable1 m) => Typeable (TemplateState m) where
    typeOf _ = mkTyConApp templateStateTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- | TemplateMonad is the monad used for 'Splice' processing.  TemplateMonad
-- provides \"passthrough\" instances for many of the monads you might use in
-- the inner monad.
newtype TemplateMonad m a = TemplateMonad {
    runTemplateMonad :: Node
                     -> TemplateState m
                     -> m (a, TemplateState m)
}


------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalTemplateMonad :: Monad m
                  => TemplateMonad m a
                  -> Node
                  -> TemplateState m
                  -> m a
evalTemplateMonad m r s = do
    (a, _) <- runTemplateMonad m r s
    return a

------------------------------------------------------------------------------
-- | Helper function for the functor instance
first :: (a -> b) -> (a, c) -> (b, c)
first f (a,b) = (f a, b)


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
liftCatch :: (m (a,TemplateState m)
              -> (e -> m (a,TemplateState m))
              -> m (a,TemplateState m))
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
liftCallCC :: ((((a,TemplateState m) -> m (b, TemplateState m))
                  -> m (a, TemplateState m))
                -> m (a, TemplateState m))
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
templateMonadTyCon = mkTyCon "Text.Templating.Heist.Types.TemplateMonad"
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
-- getChildren@ returns a list containing one 'Text' node containing part of
-- Hamlet's speech.  @getParamNode >>= getAttribute \"author\"@ would return
-- @Just "Shakespeare"@.
getParamNode :: Monad m => TemplateMonad m Node
getParamNode = TemplateMonad $ \r s -> return (r,s)


------------------------------------------------------------------------------
-- | TemplateMonad's local
localParamNode :: Monad m
               => (Node -> Node)
               -> TemplateMonad m a
               -> TemplateMonad m a
localParamNode f m = TemplateMonad $ \r s -> runTemplateMonad m (f r) s


------------------------------------------------------------------------------
-- | TemplateMonad's gets
getsTS :: Monad m => (TemplateState m -> r) -> TemplateMonad m r
getsTS f = TemplateMonad $ \_ s -> return (f s, s)


------------------------------------------------------------------------------
-- | TemplateMonad's get
getTS :: Monad m => TemplateMonad m (TemplateState m)
getTS = TemplateMonad $ \_ s -> return (s, s)


------------------------------------------------------------------------------
-- | TemplateMonad's put
putTS :: Monad m => TemplateState m -> TemplateMonad m ()
putTS s = TemplateMonad $ \_ _ -> return ((), s)


------------------------------------------------------------------------------
-- | TemplateMonad's modify
modifyTS :: Monad m
                    => (TemplateState m -> TemplateState m)
                    -> TemplateMonad m ()
modifyTS f = TemplateMonad $ \_ s -> return ((), f s)


