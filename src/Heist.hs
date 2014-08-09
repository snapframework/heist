{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-|

This module defines the core data types used by Heist.  In practice you will
also want to import one or both of "Heist.Compiled" or "Heist.Interpreted" to
get functions needed for writing splices.

The Heist template system allows you to build custom HTML and XML based markup
languages.  With Heist you can define your own domain-specific tags
implemented with Haskell and use them in your templates.

-}

module Heist
  (
  -- * Primary Heist initialization functions
    loadTemplates
  , reloadTemplates
  , addTemplatePathPrefix
  , initHeist
  , initHeistWithCacheTag
  , defaultInterpretedSplices
  , defaultLoadTimeSplices

  -- * Core Heist data types
  , SpliceConfig(..)
  , HeistConfig(..)
  , TemplateRepo
  , TemplateLocation
  , Template
  , TPath
  , MIMEType
  , DocumentFile(..)
  , AttrSplice
  , RuntimeSplice
  , Chunk
  , HeistState
  , templateNames
  , compiledTemplateNames
  , hasTemplate
  , spliceNames
  , compiledSpliceNames
  , HeistT
  , evalHeistT
  , getParamNode
  , getContext
  , getTemplateFilePath
  , localParamNode
  , getsHS
  , getHS
  , putHS
  , modifyHS
  , restoreHS
  , localHS
  , getDoc
  , getXMLDoc
  , tellSpliceError
  , orError
  , module Heist.SpliceAPI
  ) where


------------------------------------------------------------------------------
import           Control.Error
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import           Control.Monad.State
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.Foldable                 as F
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as Map
import qualified Data.HeterogeneousEnvironment as HE
import           Data.Map.Syntax
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           System.Directory.Tree
import qualified Text.XmlHtml                  as X
------------------------------------------------------------------------------
import           Heist.Common
import qualified Heist.Compiled.Internal       as C
import qualified Heist.Interpreted.Internal    as I
import           Heist.SpliceAPI
import           Heist.Splices
import           Heist.Types
------------------------------------------------------------------------------


------------------------------------------------------------------------------
type TemplateRepo = HashMap TPath DocumentFile


------------------------------------------------------------------------------
-- | An IO action for getting a template repo from this location.  By not just
-- using a directory path here, we support templates loaded from a database,
-- retrieved from the network, or anything else you can think of.
type TemplateLocation = EitherT [String] IO TemplateRepo


------------------------------------------------------------------------------
-- | The splices and templates Heist will use.  To bind a splice simply
-- include it in the appropriate place here.
data SpliceConfig m = SpliceConfig
    { scInterpretedSplices :: Splices (I.Splice m)
        -- ^ Interpreted splices are the splices that Heist has always had.
        -- They return a list of nodes and are processed at runtime.
    , scLoadTimeSplices    :: Splices (I.Splice IO)
        -- ^ Load time splices are like interpreted splices because they
        -- return a list of nodes.  But they are like compiled splices because
        -- they are processed once at load time.  All of Heist's built-in
        -- splices should be used as load time splices.
    , scCompiledSplices    :: Splices (C.Splice m)
        -- ^ Compiled splices return a DList of Chunks and are processed at
        -- load time to generate a runtime monad action that will be used to
        -- render the template.
    , scAttributeSplices   :: Splices (AttrSplice m)
        -- ^ Attribute splices are bound to attribute names and return a list
        -- of attributes.
    , scTemplateLocations  :: [TemplateLocation]
        -- ^ A list of all the locations that Heist should get its templates
        -- from.
    }


instance Monoid (SpliceConfig m) where
    mempty = SpliceConfig mempty mempty mempty mempty mempty
    mappend (SpliceConfig a1 b1 c1 d1 e1) (SpliceConfig a2 b2 c2 d2 e2) =
      SpliceConfig (mappend a1 a2) (mappend b1 b2) (mappend c1 c2)
                   (mappend d1 d2) (mappend e1 e2)


data HeistConfig m = HeistConfig
    { hcSpliceConfig  :: SpliceConfig m
        -- ^ Splices and templates
    , hcNamespace     :: Text
        -- ^ A namespace to use for all tags that are bound to splices.  Use
        -- empty string for no namespace.
    , hcErrorNotBound :: Bool
        -- ^ Whether to throw an error when a tag wih the heist namespace does
        -- not correspond to a bound splice.  When not using a namespace, this
        -- flag is ignored.
    }


------------------------------------------------------------------------------
-- | The built-in set of splices that you should use in compiled splice mode.
-- This list includes everything from 'defaultInterpretedSplices' plus a
-- splice for the content tag that errors out when it sees any instance of the
-- old content tag, which has now been moved to two separate tags called
-- apply-content and bind-content.
defaultLoadTimeSplices :: MonadIO m => Splices (I.Splice m)
defaultLoadTimeSplices = do
    -- To be removed in later versions
    defaultInterpretedSplices
    "content" #! deprecatedContentCheck



------------------------------------------------------------------------------
-- | The built-in set of static splices.  All the splices that used to be
-- enabled by default are included here.  To get the normal Heist behavior you
-- should include these in the scLoadTimeSplices list in your SpliceConfig.
-- If you are using interpreted splice mode, then you might also want to bind
-- the 'deprecatedContentCheck' splice to the content tag as a load time
-- splice.
defaultInterpretedSplices :: MonadIO m => Splices (I.Splice m)
defaultInterpretedSplices = do
    applyTag ## applyImpl
    bindTag ## bindImpl
    ignoreTag ## ignoreImpl
    markdownTag ## markdownSplice



allErrors :: [Either String (TPath, v)]
          -> EitherT [String] IO (HashMap TPath v)
allErrors tlist =
    case errs of
        [] -> right $ Map.fromList $ rights tlist
        _  -> left errs
  where
    errs = lefts tlist


------------------------------------------------------------------------------
-- | Loads templates from disk.  This function returns just a template map so
-- you can load multiple directories and combine the maps before initializing
-- your HeistState.
loadTemplates :: FilePath -> EitherT [String] IO TemplateRepo
loadTemplates dir = do
    d <- lift $ readDirectoryWith (loadTemplate dir) dir
    allErrors $ F.fold (free d)


------------------------------------------------------------------------------
-- | Reloads all the templates an an existing TemplateRepo.
reloadTemplates :: TemplateRepo -> EitherT [String] IO TemplateRepo
reloadTemplates repo = do
    tlist <- lift $ mapM loadOrKeep $ Map.toList repo
    allErrors tlist
  where
    loadOrKeep (p,df) =
      case dfFile df of
        Nothing -> return $ Right (p, df)
        Just fp -> do
          df' <- loadTemplate' fp
          return $ fmap (p,) $ case df' of
            [t] -> t
            _ -> Left "Template repo has non-templates"


------------------------------------------------------------------------------
-- | Adds a path prefix to a templates in a map returned by loadTemplates.  If
-- you want to add multiple levels of directories, separate them with slashes
-- as in "foo/bar".  Using an empty string as a path prefix will leave the
-- map unchanged.
addTemplatePathPrefix :: ByteString -> TemplateRepo -> TemplateRepo
addTemplatePathPrefix dir ts
  | B.null dir = ts
  | otherwise  = Map.fromList $
                 map (\(x,y) -> (f x, y)) $
                 Map.toList ts
  where
    f ps = ps++splitTemplatePath dir


------------------------------------------------------------------------------
-- | Creates an empty HeistState.
emptyHS :: HE.KeyGen -> HeistState m
emptyHS kg = HeistState Map.empty Map.empty Map.empty Map.empty Map.empty
                        True [] 0 [] Nothing kg False Html "" [] False


------------------------------------------------------------------------------
-- | This is the main Heist initialization function.  You pass in a map of all
-- templates and all of your splices and it constructs and returns a
-- HeistState.
--
-- We don't provide functions to add either type of loadtime splices to your
-- HeistState after initHeist because it doesn't make any sense unless you
-- re-initialize all templates with the new splices.  If you add any old-style
-- runtime heist splices after calling this function, they will still work
-- fine if you use Heist.Interpreted.renderTemplate.  If you add any templates
-- later, then those templates will be available for interpreted rendering,
-- but not for compiled rendering.
--
-- In the past you could add templates to your HeistState after initialization
-- using its Monoid instance.  Due to implementation details, this is no
-- longer possible.  All of your templates must be known when you call this
-- function.
initHeist :: Monad n
          => HeistConfig n
          -> EitherT [String] IO (HeistState n)
initHeist hc = do
    keyGen <- lift HE.newKeyGen
    repos <- sequence $ scTemplateLocations $ hcSpliceConfig hc
    initHeist' keyGen hc (Map.unions repos)


------------------------------------------------------------------------------
mkSplicePrefix :: Text -> Text
mkSplicePrefix ns
  | T.null ns = ""
  | otherwise = ns `mappend` ":"


------------------------------------------------------------------------------
initHeist' :: Monad n
           => HE.KeyGen
           -> HeistConfig n
           -> TemplateRepo
           -> EitherT [String] IO (HeistState n)
initHeist' keyGen (HeistConfig sc ns enn) repo = do
    let empty = emptyHS keyGen
    let (SpliceConfig i lt c a _) = sc
    tmap <- preproc keyGen lt repo ns
    let prefix = mkSplicePrefix ns
    is <- runHashMap $ mapK (prefix<>) i
    cs <- runHashMap $ mapK (prefix<>) c
    as <- runHashMap $ mapK (prefix<>) a
    let hs1 = empty { _spliceMap = is
                    , _templateMap = tmap
                    , _compiledSpliceMap = cs
                    , _attrSpliceMap = as
                    , _splicePrefix = prefix
                    , _errorNotBound = enn
                    }
    EitherT $ C.compileTemplates hs1
--    liftIO $ when (not $ null $ _spliceErrors hs2) $ do
--        putStrLn "Finished compiling with errors..."
--        mapM_ T.putStrLn $ _spliceErrors hs2
--    case _spliceErrors hs2 of
--      [] -> return hs2
--      es -> left $ map T.unpack es


------------------------------------------------------------------------------
-- | Runs preprocess on a TemplateRepo and returns the modified templates.
preproc :: HE.KeyGen
        -> Splices (I.Splice IO)
        -> TemplateRepo
        -> Text
        -> EitherT [String] IO TemplateRepo
preproc keyGen splices templates ns = do
    sm <- runHashMap splices
    let hs = (emptyHS keyGen) { _spliceMap = sm
                              , _templateMap = templates
                              , _preprocessingMode = True
                              , _splicePrefix = mkSplicePrefix ns }
    let eval a = evalHeistT a (X.TextNode "") hs
    tPairs <- lift $ mapM (eval . preprocess) $ Map.toList templates
    let bad = lefts tPairs
    if not (null bad)
      then left bad
      else right $ Map.fromList $ rights tPairs


------------------------------------------------------------------------------
-- | Processes a single template, running load time splices.
preprocess :: (TPath, DocumentFile)
           -> HeistT IO IO (Either String (TPath, DocumentFile))
preprocess (tpath, docFile) = do
    let tname = tpathName tpath
    !emdoc <- try $ I.evalWithDoctypes tname
              :: HeistT IO IO (Either SomeException (Maybe X.Document))
    let f !doc = (tpath, docFile { dfDoc = doc })
    return $! either (Left . show) (Right . maybe die f) emdoc
  where
    die = error "Preprocess didn't succeed!  This should never happen."


------------------------------------------------------------------------------
-- | Wrapper around initHeist that also sets up a cache tag.  It sets up both
-- compiled and interpreted versions of the cache tag splices.  If you need to
-- configure the cache tag differently than how this function does it, you
-- will still probably want to pattern your approach after this function's
-- implementation.
initHeistWithCacheTag :: MonadIO n
                      => HeistConfig n
                      -> EitherT [String] IO (HeistState n, CacheTagState)
initHeistWithCacheTag (HeistConfig sc ns enn) = do
    (ss, cts) <- liftIO mkCacheTag
    let tag = "cache"
    keyGen <- lift HE.newKeyGen

    repos <- sequence $ scTemplateLocations sc
    -- We have to do one preprocessing pass with the cache setup splice.  This
    -- has to happen for both interpreted and compiled templates, so we do it
    -- here by itself because interpreted templates don't get the same load
    -- time splices as compiled templates.
    rawWithCache <- preproc keyGen (tag ## ss) (Map.unions repos) ns

    let sc' = SpliceConfig (tag #! cacheImpl cts) mempty
                           (tag #! cacheImplCompiled cts) mempty mempty
    let hc = HeistConfig (mappend sc sc') ns enn
    hs <- initHeist' keyGen hc rawWithCache
    return (hs, cts)

