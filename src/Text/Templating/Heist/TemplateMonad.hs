{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Templating.Heist.TemplateMonad where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid
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


newtype TemplateMonad m a = TemplateMonad {
    runTemplateMonad :: Node
                     -> (TemplateState m)
                     -> m (a, TemplateState m)
}

first :: (a -> b) -> (a, c) -> (b, c)
first f (a,b) = (f a, b)

instance Functor m => Functor (TemplateMonad m) where
    fmap f (TemplateMonad m) = TemplateMonad $ \r s -> first f <$> m r s

instance (Monad m, Functor m) => Applicative (TemplateMonad m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (TemplateMonad m) where
    return a = TemplateMonad (\_ s -> return (a, s))
    TemplateMonad m >>= k = TemplateMonad $ \r s -> do
        (a, s') <- m r s
        runTemplateMonad (k a) r s'

instance MonadIO m => MonadIO (TemplateMonad m) where
    liftIO = lift . liftIO

instance MonadTrans TemplateMonad where
    lift m = TemplateMonad $ \_ s -> do
        a <- m
        return (a, s)

instance MonadFix m => MonadFix (TemplateMonad m) where
    mfix f = TemplateMonad $ \r s ->
        mfix $ \ (a, _) -> runTemplateMonad (f a) r s

instance (Functor m, MonadPlus m) => Alternative (TemplateMonad m) where
    empty = mzero
    (<|>) = mplus

instance MonadPlus m => MonadPlus (TemplateMonad m) where
    mzero = lift mzero
    m `mplus` n = TemplateMonad $ \r s ->
        runTemplateMonad m r s `mplus` runTemplateMonad n r s

instance MonadState s m => MonadState s (TemplateMonad m) where
    get = lift get
    put = lift . put

instance MonadReader r m => MonadReader r (TemplateMonad m) where
    ask = TemplateMonad $ \_ s -> do
            r <- ask
            return (r,s)
    local f (TemplateMonad m) =
        TemplateMonad $ \r s -> local f (m r s)

--instance MonadError m => MonadError (TemplateMonad m) where
--    throwError = lift . throwError
--    catchError = liftCatch catchError


------------------------------------------------------------------------------
-- Functions for our monad.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | 
getParamNode :: Monad m => TemplateMonad m Node
getParamNode = TemplateMonad $ \r s -> return (r,s)


------------------------------------------------------------------------------
-- | 
localParamNode :: Monad m
               => (Node -> Node)
               -> TemplateMonad m a
               -> TemplateMonad m a
localParamNode f m = TemplateMonad $ \r s -> runTemplateMonad m (f r) s


------------------------------------------------------------------------------
-- | 
getsTemplateState :: Monad m => (TemplateState m -> r) -> TemplateMonad m r
getsTemplateState f = TemplateMonad $ \_ s -> return (f s, s)


------------------------------------------------------------------------------
-- | 
getTemplateState :: Monad m => TemplateMonad m (TemplateState m)
getTemplateState = TemplateMonad $ \_ s -> return (s, s)


------------------------------------------------------------------------------
-- | 
putTemplateState :: Monad m => TemplateState m -> TemplateMonad m ()
putTemplateState s = TemplateMonad $ \_ _ -> return ((), s)


------------------------------------------------------------------------------
-- | 
modifyTemplateState :: Monad m
                    => (TemplateState m -> TemplateState m)
                    -> TemplateMonad m ()
modifyTemplateState f = TemplateMonad $ \_ s -> return ((), f s)


