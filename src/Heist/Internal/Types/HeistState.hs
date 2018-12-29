{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ExistentialQuantification  #-}
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
import           Control.Exception             (Exception)
import           Control.Monad                 (MonadPlus (..), ap)
import           Control.Monad.Base
import           Control.Monad.Cont            (MonadCont (..))
#if MIN_VERSION_mtl(2,2,1)
import           Control.Monad.Except          (MonadError (..))
#else
import           Control.Monad.Error           (MonadError (..))
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
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
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
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
instance (Monad m, Semigroup a) => Semigroup (RuntimeSplice m a) where
    a <> b = do
        !x <- a
        !y <- b
        return $! x <> y


#if !MIN_VERSION_base(4,11,0)
instance (Monad m, Semigroup a, Monoid a) => Monoid (RuntimeSplice m a) where
#else
instance (Monad m, Monoid a) => Monoid (RuntimeSplice m a) where
#endif
    mempty = return mempty
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif


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
    , contextNode        :: X.Node
    , spliceMsg          :: Text
    } deriving ( Show, Eq )


------------------------------------------------------------------------------
-- | Transform a SpliceError record to a Text message.
spliceErrorText :: SpliceError -> Text
spliceErrorText (SpliceError hist tf splices node msg) =
    (maybe "" ((`mappend` ": ") . T.pack) tf) `T.append` msg `T.append`
    foldr (\(_, tf', tag) -> (("\n   ... via " `T.append`
                               (maybe "" ((`mappend` ": ") . T.pack) tf')
                               `T.append` tag) `T.append`)) T.empty hist
    `T.append`
    if null splices
      then T.empty
      else "\nBound splices:" `T.append`
         foldl (\x y -> x `T.append` " " `T.append` y) T.empty splices
    `T.append`
    (T.pack $ "\nNode: " ++ (show node))


------------------------------------------------------------------------------
-- | Exception type for splice compile errors.  Wraps the original
-- exception and provides context.
--data (Exception e) => CompileException e = CompileException
data CompileException = forall e . Exception e => CompileException
    { originalException :: e
    -- The list of splice errors.  The head of it has the context
    -- related to the exception.
    , exceptionContext :: [SpliceError]
    } deriving ( Typeable )


instance Show CompileException where
    show (CompileException e []) =
      "Heist load exception (unknown context): " ++ (show e)
    show (CompileException _ (c:_)) = (T.unpack $ spliceErrorText c)


instance Exception CompileException


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @HeistState@ using 'initHeist' and any of Heist's @HeistState ->
-- HeistState@ \"filter\" functions.  Then you use the resulting @HeistState@
-- in calls to 'renderTemplate'.
--
-- n and m are the runtime and load time monads, respectively
data HeistState n m = HeistState {
    -- | A mapping of splice names to splice actions
      _spliceMap           :: HashMap Text (HeistT n n Template)
    -- | A mapping of template names to templates
    , _templateMap         :: HashMap TPath DocumentFile

    -- | A mapping of splice names to splice actions
    , _compiledSpliceMap   :: HashMap Text (HeistT n m (DList (Chunk n)))
    -- | A mapping of template names to templates
    --, _compiledTemplateMap :: HashMap TPath (m Builder, MIMEType)
    , _compiledTemplateMap :: !(HashMap TPath ([Chunk n], MIMEType))

    , _attrSpliceMap       :: HashMap Text (AttrSplice n)

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
-- @n@ is the runtime monad (the first parameter to HeistState).
--
-- @m@ is the monad being run now.  In this case, \"now\" is a variable
-- concept.  The type @HeistT n n@ means that \"now\" is runtime.  The type
-- @HeistT n IO@ means that \"now\" is @IO@, and more importantly it is NOT
-- runtime. In Heist, the rule of thumb is that @IO@ means load time and @n@
-- means runtime.
type HeistT n m a = GHeistT n m m a


------------------------------------------------------------------------------
-- | GHeistT is the internal type for HeistT with the @m@ parameter
-- decoupled from the underlying monad @t@.
newtype GHeistT n m t a = GHeistT {
    runHeistT :: X.Node
              -> HeistState n m
              -> t (a, HeistState n m)
#if MIN_VERSION_base(4,7,0)
} deriving Typeable
#else
}
#endif




------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
templateNames :: HeistState n m -> [TPath]
templateNames ts = H.keys $ _templateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
compiledTemplateNames :: HeistState n m -> [TPath]
compiledTemplateNames ts = H.keys $ _compiledTemplateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the interpreted splices defined in a HeistState.
spliceNames :: HeistState n m -> [Text]
spliceNames ts = H.keys $ _spliceMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the compiled splices defined in a HeistState.
compiledSpliceNames :: HeistState n m -> [Text]
compiledSpliceNames HeistState {_compiledSpliceMap = m} = H.keys m


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
           => GHeistT n m m a
           -> X.Node
           -> HeistState n m
           -> m a
evalHeistT m r s = do
    (a, _) <- runHeistT m r s
    return a
{-# INLINE evalHeistT #-}


------------------------------------------------------------------------------
-- | Functor instance
instance Functor m => Functor (GHeistT n m m) where
    fmap f (GHeistT m) = GHeistT $ \r s -> first f <$> m r s


------------------------------------------------------------------------------
-- | Applicative instance
instance (Monad m, Functor m) => Applicative (GHeistT n m m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
-- | Monad instance
instance Monad m => Monad (GHeistT n m m) where
    return a = GHeistT (\_ s -> return (a, s))
    {-# INLINE return #-}
    GHeistT m >>= k = GHeistT $ \r s -> do
        (a, s') <- m r s
        runHeistT (k a) r s'
    {-# INLINE (>>=) #-}


#if MIN_VERSION_base(4,9,0)
------------------------------------------------------------------------------
-- | MonadFail instance
instance Fail.MonadFail m => Fail.MonadFail (GHeistT n m m) where
    fail = lift . Fail.fail
#endif


------------------------------------------------------------------------------
-- | MonadIO instance
instance MonadIO m => MonadIO (GHeistT n m m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
-- | MonadTrans instance
instance MonadTrans (GHeistT n m) where
    lift m = GHeistT $ \_ s -> do
        a <- m
        return (a, s)


instance MonadBase b m => MonadBase b (GHeistT n m m) where
    liftBase = lift . liftBase

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl (GHeistT n m) where
    type StT (GHeistT n m) a = (a, HeistState n m)
    liftWith f = GHeistT $ \n s -> do
        res <- f (\(GHeistT g) -> g n s)
        return (res, s)
    restoreT k = GHeistT $ \_ _ -> k
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


instance MonadBaseControl b m => MonadBaseControl b (GHeistT n m m) where
     type StM (GHeistT n m m) a = ComposeSt (GHeistT n m) m a
     liftBaseWith = defaultLiftBaseWith
     restoreM = defaultRestoreM
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}
#else
instance MonadTransControl (GHeistT n m) where
    newtype StT (GHeistT n m) a = StHeistT {unStHeistT :: (a, HeistState n m)}
    liftWith f = GHeistT $ \n s -> do
        res <- f $ \(GHeistT g) -> liftM StHeistT $ g n s
        return (res, s)
    restoreT k = GHeistT $ \_ _ -> liftM unStHeistT k
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


instance MonadBaseControl b m => MonadBaseControl b (GHeistT n m m) where
     newtype StM (GHeistT n m m) a = StMHeist {unStMHeist :: ComposeSt (GHeistT n m) m a}
     liftBaseWith = defaultLiftBaseWith StMHeist
     restoreM = defaultRestoreM unStMHeist
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}
#endif

------------------------------------------------------------------------------
-- | MonadFix passthrough instance
instance MonadFix m => MonadFix (GHeistT n m m) where
    mfix f = GHeistT $ \r s ->
        mfix $ \ (a, _) -> runHeistT (f a) r s


------------------------------------------------------------------------------
-- | Alternative passthrough instance
instance (Functor m, MonadPlus m) => Alternative (GHeistT n m m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
-- | MonadPlus passthrough instance
instance MonadPlus m => MonadPlus (GHeistT n m m) where
    mzero = lift mzero
    m `mplus` n = GHeistT $ \r s ->
        runHeistT m r s `mplus` runHeistT n r s


------------------------------------------------------------------------------
-- | MonadState passthrough instance
instance MonadState s m => MonadState s (GHeistT n m m) where
    get = lift get
    {-# INLINE get #-}
    put = lift . put
    {-# INLINE put #-}


------------------------------------------------------------------------------
-- | MonadReader passthrough instance
instance MonadReader r m => MonadReader r (GHeistT n m m) where
    ask = GHeistT $ \_ s -> do
            r <- ask
            return (r,s)
    local f (GHeistT m) =
        GHeistT $ \r s -> local f (m r s)


------------------------------------------------------------------------------
-- | Helper for MonadError instance.
_liftCatch
    :: (m (a,HeistState n m)
        -> (e -> m (a,HeistState n m))
        -> m (a,HeistState n m))
    -> GHeistT n m m a
    -> (e -> GHeistT n m m a)
    -> GHeistT n m m a
_liftCatch ce m h =
    GHeistT $ \r s ->
        (runHeistT m r s `ce`
        (\e -> runHeistT (h e) r s))


------------------------------------------------------------------------------
-- | MonadError passthrough instance
instance (MonadError e m) => MonadError e (GHeistT n m m) where
    throwError = lift . throwError
    catchError = _liftCatch catchError


------------------------------------------------------------------------------
-- | Helper for MonadCont instance.
_liftCallCC
    :: ((((a,HeistState n m) -> m (b, HeistState n m))
           -> m (a, HeistState n m))
         -> m (a, HeistState n m))
    -> ((a -> GHeistT n m m b) -> GHeistT n m m a)
    -> GHeistT n m m a
_liftCallCC ccc f = GHeistT $ \r s ->
    ccc $ \c ->
    runHeistT (f (\a -> GHeistT $ \_ _ -> c (a, s))) r s


------------------------------------------------------------------------------
-- | MonadCont passthrough instance
instance (MonadCont m) => MonadCont (GHeistT n m m) where
    callCC = _liftCallCC callCC


#if !MIN_VERSION_base(4,7,0)
------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Heist.HeistT"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (GHeistT n m m) where
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
getParamNode :: Monad m => GHeistT n m m X.Node
getParamNode = GHeistT $ curry return
{-# INLINE getParamNode #-}


------------------------------------------------------------------------------
-- | GHeistTs 'local'.
localParamNode :: Monad m
               => (X.Node -> X.Node)
               -> GHeistT n m m a
               -> GHeistT n m m a
localParamNode f m = GHeistT $ \r s -> runHeistT m (f r) s
{-# INLINE localParamNode #-}


------------------------------------------------------------------------------
-- | GHeistTs 'gets'.
getsHS :: Monad m => (HeistState n m -> r) -> GHeistT n m m r
getsHS f = GHeistT $ \_ s -> return (f s, s)
{-# INLINE getsHS #-}


------------------------------------------------------------------------------
-- | GHeistTs 'get'.
getHS :: Monad m => GHeistT n m m (HeistState n m)
getHS = GHeistT $ \_ s -> return (s, s)
{-# INLINE getHS #-}


------------------------------------------------------------------------------
-- | GHeistTs 'put'.
putHS :: Monad m => HeistState n m -> GHeistT n m m ()
putHS s = GHeistT $ \_ _ -> return ((), s)
{-# INLINE putHS #-}


------------------------------------------------------------------------------
-- | GHeistTs 'modify'.
modifyHS :: Monad m
         => (HeistState n m -> HeistState n m)
         -> GHeistT n m m ()
modifyHS f = GHeistT $ \_ s -> return ((), f s)
{-# INLINE modifyHS #-}


------------------------------------------------------------------------------
-- | Restores the HeistState.  This function is almost like putHS except it
-- preserves the current doctypes and splice errors.  You should use this
-- function instead of @putHS@ to restore an old state.  This was needed
-- because doctypes needs to be in a "global scope" as opposed to the template
-- call "local scope" of state items such as recursionDepth, curContext, and
-- spliceMap.
restoreHS :: Monad m => HeistState n m -> GHeistT n m m ()
restoreHS old = modifyHS (\cur -> old { _doctypes = _doctypes cur
                                      , _numNamespacedTags =
                                        _numNamespacedTags cur
                                      , _spliceErrors = _spliceErrors cur })
{-# INLINE restoreHS #-}


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a HeistT computation with
-- a modified heist state.
localHS :: Monad m
        => (HeistState n m -> HeistState n m)
        -> GHeistT n m m a
        -> GHeistT n m m a
localHS f k = do
    ts <- getHS
    putHS $ f ts
    res <- k
    restoreHS ts
    return res
{-# INLINE localHS #-}


------------------------------------------------------------------------------
-- | Modifies the recursion depth.
modRecursionDepth :: Monad m => (Int -> Int) -> GHeistT n m m ()
modRecursionDepth f =
    modifyHS (\st -> st { _recursionDepth = f (_recursionDepth st) })


------------------------------------------------------------------------------
-- | Increments the namespaced tag count
incNamespacedTags :: Monad m => GHeistT n m m ()
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


