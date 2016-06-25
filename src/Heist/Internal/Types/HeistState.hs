{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|

This module contains the core Heist data types.

Edward Kmett wrote most of the HeistT monad code and associated instances,
liberating us from the unused writer portion of RWST.

-}

module Heist.Internal.Types.HeistState where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder      (Builder)
import           Control.Applicative           (Alternative (..))
import           Control.Arrow                 (first)
import           Control.Monad                 (MonadPlus (..), ap)
import           Control.Monad.Base
import           Control.Monad.Cont            (MonadCont (..))
#if MIN_VERSION_mtl(2,2,1)
import           Control.Monad.Except          (MonadError (..))
#else
import           Control.Monad.Error           (MonadError (..))
#endif
import           Control.Monad.Fix             (MonadFix (..))
import           Control.Monad.Reader          (MonadReader (..))
import           Control.Monad.State.Strict    (MonadState (..), StateT)
import           Control.Monad.Trans           (MonadIO (..), MonadTrans (..))
import           Control.Monad.Trans.Control
import           Data.ByteString.Char8         (ByteString)
import           Data.DList                    (DList)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as H
import           Data.HeterogeneousEnvironment (HeterogeneousEnvironment)
import qualified Data.HeterogeneousEnvironment as HE
import           Data.Map.Syntax
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
#if MIN_VERSION_base (4,7,0)
import           Data.Typeable                 (Typeable)
#else
import           Data.Typeable                 (TyCon, Typeable(..),
                                                Typeable1(..), mkTyCon,
                                                mkTyConApp)
#endif

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative           (Applicative (..), (<$>))
import           Data.Monoid                   (Monoid(..))
#endif

import qualified Text.XmlHtml                  as X
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Convenient type alies for splices.
type Splices s = MapSyntax Text s


------------------------------------------------------------------------------
-- | A 'Template' is a forest of XML nodes.  Here we deviate from the \"single
-- root node\" constraint of well-formed XML because we want to allow
-- templates to contain document fragments that may not have a single root.
type Template = [X.Node]


------------------------------------------------------------------------------
-- | MIME Type.  The type alias is here to make the API clearer.
type MIMEType = ByteString


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
-- currently being processed.
type TPath = [ByteString]


------------------------------------------------------------------------------
-- | Holds data about templates read from disk.
data DocumentFile = DocumentFile
    { dfDoc  :: X.Document
    , dfFile :: Maybe FilePath
    } deriving ( Eq, Show
#if MIN_VERSION_base(4,7,0)
               , Typeable
#endif
               )


------------------------------------------------------------------------------
-- | Designates whether a document should be treated as XML or HTML.
data Markup = Xml | Html


------------------------------------------------------------------------------
-- | Monad used for runtime splice execution.
newtype RuntimeSplice m a = RuntimeSplice {
      unRT :: StateT HeterogeneousEnvironment m a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadState HeterogeneousEnvironment
               , MonadTrans
#if MIN_VERSION_base(4,7,0)
               , Typeable
#endif
               )


------------------------------------------------------------------------------
instance (Monad m, Monoid a) => Monoid (RuntimeSplice m a) where
    mempty = return mempty

    a `mappend` b = do
        !x <- a
        !y <- b
        return $! x `mappend` y


------------------------------------------------------------------------------
-- | Opaque type representing pieces of output from compiled splices.
data Chunk m = Pure !ByteString
               -- ^ output known at load time
             | RuntimeHtml !(RuntimeSplice m Builder)
               -- ^ output computed at run time
             | RuntimeAction !(RuntimeSplice m ())
               -- ^ runtime action used only for its side-effect
#if MIN_VERSION_base(4,7,0)
             deriving Typeable
#endif

instance Show (Chunk m) where
    show (Pure _) = "Pure"
    show (RuntimeHtml _) = "RuntimeHtml"
    show (RuntimeAction _) = "RuntimeAction"


showChunk :: Chunk m -> String
showChunk (Pure b) = T.unpack $ decodeUtf8 b
showChunk (RuntimeHtml _) = "RuntimeHtml"
showChunk (RuntimeAction _) = "RuntimeAction"


isPureChunk :: Chunk m -> Bool
isPureChunk (Pure _) = True
isPureChunk _ = False


------------------------------------------------------------------------------
-- | Type alias for attribute splices.  The function parameter is the value of
-- the bound attribute splice.  The return value is a list of attribute
-- key/value pairs that get substituted in the place of the bound attribute.
type AttrSplice m = Text -> RuntimeSplice m [(Text, Text)]


------------------------------------------------------------------------------
-- | Detailed information about a splice error.
data SpliceError = SpliceError
    { spliceHistory      :: [(TPath, Maybe FilePath, Text)]
    , spliceTemplateFile :: Maybe FilePath
    , visibleSplices     :: [Text]
    , spliceMsg          :: Text
    } deriving ( Show, Eq )


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @HeistState@ using 'initHeist' and any of Heist's @HeistState ->
-- HeistState@ \"filter\" functions.  Then you use the resulting @HeistState@
-- in calls to 'renderTemplate'.
--
-- m is the runtime monad
data HeistState m = HeistState {
    -- | A mapping of splice names to splice actions
      _spliceMap           :: HashMap Text (HeistT m m Template)
    -- | A mapping of template names to templates
    , _templateMap         :: HashMap TPath DocumentFile

    -- | A mapping of splice names to splice actions
    , _compiledSpliceMap   :: HashMap Text (HeistT m IO (DList (Chunk m)))
    -- | A mapping of template names to templates
    --, _compiledTemplateMap :: HashMap TPath (m Builder, MIMEType)
    , _compiledTemplateMap :: !(HashMap TPath ([Chunk m], MIMEType))

    , _attrSpliceMap       :: HashMap Text (AttrSplice m)

    -- | A flag to control splice recursion
    , _recurse             :: Bool
    -- | The path to the template currently being processed.
    , _curContext          :: TPath
    -- | Stack of the splices used.
    , _splicePath          :: [(TPath, Maybe FilePath, Text)]
    -- | A counter keeping track of the current recursion depth to prevent
    -- infinite loops.
    , _recursionDepth      :: Int
    -- | The doctypes encountered during template processing.
    , _doctypes            :: [X.DocType]
    -- | The full path to the current template's file on disk.
    , _curTemplateFile     :: Maybe FilePath
    -- | A key generator used to produce new unique Promises.
    , _keygen              :: HE.KeyGen

    -- | Flag indicating whether we're in preprocessing mode.  During
    -- preprocessing, errors should stop execution and be reported.  During
    -- template rendering, it's better to skip the errors and render the page.
    , _preprocessingMode   :: Bool

    -- | This is needed because compiled templates are generated with a bunch
    -- of calls to renderFragment rather than a single call to render.
    , _curMarkup           :: Markup

    -- | A prefix for all splices (namespace ++ ":").
    , _splicePrefix        :: Text

    -- | List of errors encountered during splice processing.
    , _spliceErrors        :: [SpliceError]

    -- | Whether to throw an error when a tag wih the heist namespace does not
    -- correspond to a bound splice.  When not using a namespace, this flag is
    -- ignored.
    , _errorNotBound       :: Bool
    , _numNamespacedTags   :: Int
#if MIN_VERSION_base(4,7,0)
} deriving (Typeable)
#else
}
#endif

#if !MIN_VERSION_base(4,7,0)
-- NOTE: We got rid of the Monoid instance because it is absolutely not safe
-- to combine two compiledTemplateMaps.  All compiled templates must be known
-- at load time and processed in a single call to initHeist/loadTemplates or
-- whatever we end up calling it..

instance (Typeable1 m) => Typeable (HeistState m) where
    typeOf _ = mkTyConApp templateStateTyCon [typeOf1 (undefined :: m ())]

#endif

------------------------------------------------------------------------------
-- | HeistT is the monad transformer used for splice processing.  HeistT
-- intentionally does not expose any of its functionality via MonadState or
-- MonadReader functions.  We define passthrough instances for the most common
-- types of monads.  These instances allow the user to use HeistT in a monad
-- stack without needing calls to `lift`.
--
-- @n@ is the runtime monad (the parameter to HeistState).
--
-- @m@ is the monad being run now.  In this case, \"now\" is a variable
-- concept.  The type @HeistT n n@ means that \"now\" is runtime.  The type
-- @HeistT n IO@ means that \"now\" is @IO@, and more importantly it is NOT
-- runtime. In Heist, the rule of thumb is that @IO@ means load time and @n@
-- means runtime.
newtype HeistT n m a = HeistT {
    runHeistT :: X.Node
              -> HeistState n
              -> m (a, HeistState n)
#if MIN_VERSION_base(4,7,0)
} deriving Typeable
#else
}
#endif


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
templateNames :: HeistState m -> [TPath]
templateNames ts = H.keys $ _templateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
compiledTemplateNames :: HeistState m -> [TPath]
compiledTemplateNames ts = H.keys $ _compiledTemplateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the interpreted splices defined in a HeistState.
spliceNames :: HeistState m -> [Text]
spliceNames ts = H.keys $ _spliceMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the compiled splices defined in a HeistState.
compiledSpliceNames :: HeistState m -> [Text]
compiledSpliceNames ts = H.keys $ _compiledSpliceMap ts


#if !MIN_VERSION_base(4,7,0)
------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateStateTyCon :: TyCon
templateStateTyCon = mkTyCon "Heist.HeistState"
{-# NOINLINE templateStateTyCon #-}
#endif


------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalHeistT :: (Monad m)
           => HeistT n m a
           -> X.Node
           -> HeistState n
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


instance MonadBase b m => MonadBase b (HeistT n m) where
    liftBase = lift . liftBase

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl (HeistT n) where
    type StT (HeistT n) a = (a, HeistState n)
    liftWith f = HeistT $ \n s -> do
        res <- f $ \(HeistT g) -> g n s
        return (res, s)
    restoreT k = HeistT $ \_ _ -> k
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


instance MonadBaseControl b m => MonadBaseControl b (HeistT n m) where
     type StM (HeistT n m) a = ComposeSt (HeistT n) m a
     liftBaseWith = defaultLiftBaseWith
     restoreM = defaultRestoreM
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}
#else
instance MonadTransControl (HeistT n) where
    newtype StT (HeistT n) a = StHeistT {unStHeistT :: (a, HeistState n)}
    liftWith f = HeistT $ \n s -> do
        res <- f $ \(HeistT g) -> liftM StHeistT $ g n s
        return (res, s)
    restoreT k = HeistT $ \_ _ -> liftM unStHeistT k
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


instance MonadBaseControl b m => MonadBaseControl b (HeistT n m) where
     newtype StM (HeistT n m) a = StMHeist {unStMHeist :: ComposeSt (HeistT n) m a}
     liftBaseWith = defaultLiftBaseWith StMHeist
     restoreM = defaultRestoreM unStMHeist
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}
#endif

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
_liftCatch
    :: (m (a,HeistState n)
        -> (e -> m (a,HeistState n))
        -> m (a,HeistState n))
    -> HeistT n m a
    -> (e -> HeistT n m a)
    -> HeistT n m a
_liftCatch ce m h =
    HeistT $ \r s ->
        (runHeistT m r s `ce`
        (\e -> runHeistT (h e) r s))


------------------------------------------------------------------------------
-- | MonadError passthrough instance
instance (MonadError e m) => MonadError e (HeistT n m) where
    throwError = lift . throwError
    catchError = _liftCatch catchError


------------------------------------------------------------------------------
-- | Helper for MonadCont instance.
_liftCallCC
    :: ((((a,HeistState n) -> m (b, HeistState n))
           -> m (a, HeistState n))
         -> m (a, HeistState n))
    -> ((a -> HeistT n m b) -> HeistT n m a)
    -> HeistT n m a
_liftCallCC ccc f = HeistT $ \r s ->
    ccc $ \c ->
    runHeistT (f (\a -> HeistT $ \_ _ -> c (a, s))) r s


------------------------------------------------------------------------------
-- | MonadCont passthrough instance
instance (MonadCont m) => MonadCont (HeistT n m) where
    callCC = _liftCallCC callCC


#if !MIN_VERSION_base(4,7,0)
------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Heist.HeistT"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (HeistT n m) where
    typeOf1 _ = mkTyConApp templateMonadTyCon [typeOf1 (undefined :: m ())]
#endif


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
-- return @Just \"Shakespeare\"@.
getParamNode :: Monad m => HeistT n m X.Node
getParamNode = HeistT $ curry return
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
getsHS :: Monad m => (HeistState n -> r) -> HeistT n m r
getsHS f = HeistT $ \_ s -> return (f s, s)
{-# INLINE getsHS #-}


------------------------------------------------------------------------------
-- | HeistT's 'get'.
getHS :: Monad m => HeistT n m (HeistState n)
getHS = HeistT $ \_ s -> return (s, s)
{-# INLINE getHS #-}


------------------------------------------------------------------------------
-- | HeistT's 'put'.
putHS :: Monad m => HeistState n -> HeistT n m ()
putHS s = HeistT $ \_ _ -> return ((), s)
{-# INLINE putHS #-}


------------------------------------------------------------------------------
-- | HeistT's 'modify'.
modifyHS :: Monad m
         => (HeistState n -> HeistState n)
         -> HeistT n m ()
modifyHS f = HeistT $ \_ s -> return ((), f s)
{-# INLINE modifyHS #-}


------------------------------------------------------------------------------
-- | Restores the HeistState.  This function is almost like putHS except it
-- preserves the current doctypes and splice errors.  You should use this
-- function instead of @putHS@ to restore an old state.  This was needed
-- because doctypes needs to be in a "global scope" as opposed to the template
-- call "local scope" of state items such as recursionDepth, curContext, and
-- spliceMap.
restoreHS :: Monad m => HeistState n -> HeistT n m ()
restoreHS old = modifyHS (\cur -> old { _doctypes = _doctypes cur
                                      , _spliceErrors = _spliceErrors cur })
{-# INLINE restoreHS #-}


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a HeistT computation with
-- a modified heist state.
localHS :: Monad m
        => (HeistState n -> HeistState n)
        -> HeistT n m a
        -> HeistT n m a
localHS f k = do
    ts <- getHS
    putHS $ f ts
    res <- k
    restoreHS ts
    return res
{-# INLINE localHS #-}


------------------------------------------------------------------------------
-- | Modifies the recursion depth.
modRecursionDepth :: Monad m => (Int -> Int) -> HeistT n m ()
modRecursionDepth f =
    modifyHS (\st -> st { _recursionDepth = f (_recursionDepth st) })


------------------------------------------------------------------------------
-- | Increments the namespaced tag count
incNamespacedTags :: Monad m => HeistT n m ()
incNamespacedTags =
    modifyHS (\st -> st { _numNamespacedTags = _numNamespacedTags st + 1 })


------------------------------------------------------------------------------
-- | AST to hold attribute parsing structure.  This is necessary because
-- attoparsec doesn't support parsers running in another monad.
data AttAST = Literal Text
            | Ident   Text
  deriving (Show)


------------------------------------------------------------------------------
isIdent :: AttAST -> Bool
isIdent (Ident _) = True
isIdent _         = False


