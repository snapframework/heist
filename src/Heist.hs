{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , addTemplatePathPrefix
  , initHeist
  , initHeistWithCacheTag
  , defaultInterpretedSplices
  , defaultLoadTimeSplices

  -- * Core Heist data types
  , Template
  , TPath
  , HeistConfig(..)
  , MIMEType
  , DocumentFile(..)
  , AttrSplice
  , RuntimeSplice
  , Chunk
  , HeistState(..)
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
  , orError
  ) where

import           Control.Error
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.HeterogeneousEnvironment   as HE
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Monoid
import           Data.Text                       (Text)
import           System.Directory.Tree
import qualified Text.XmlHtml                    as X

import           Heist.Common
import qualified Heist.Compiled.Internal as C
import qualified Heist.Interpreted.Internal as I
import           Heist.Splices
import           Heist.Types


type TemplateRepo = HashMap TPath DocumentFile

data HeistConfig m = HeistConfig
    { hcInterpretedSplices :: [(Text, I.Splice m)]
    -- ^ Interpreted splices are the splices that Heist has always had.  They
    -- return a list of nodes and are processed at runtime.
    , hcLoadTimeSplices    :: [(Text, I.Splice IO)]
    -- ^ Load time splices are like interpreted splices because they return a
    -- list of nodes.  But they are like compiled splices because they are
    -- processed once at load time.  All of Heist's built-in splices should be
    -- used as load time splices.
    , hcCompiledSplices    :: [(Text, C.Splice m)]
    -- ^ Compiled splices return a DList of Chunks and are processed at load
    -- time to generate a runtime monad action that will be used to render the
    -- template.
    , hcAttributeSplices   :: [(Text, AttrSplice m)]
    -- ^ Attribute splices are bound to attribute names and return a list of
    -- attributes.
    , hcTemplates          :: TemplateRepo
    -- ^ Templates returned from the 'loadTemplates' function.
    }


instance Monoid (HeistConfig m) where
    mempty = HeistConfig [] [] [] [] Map.empty
    mappend (HeistConfig a b c d e) (HeistConfig a' b' c' d' e') =
        HeistConfig (a `mappend` a')
                    (b `mappend` b')
                    (c `mappend` c')
                    (d `mappend` d')
                    (e `mappend` e')


------------------------------------------------------------------------------
-- | The built-in set of static splices.  All the splices that used to be
-- enabled by default are included here.  To get the normal Heist behavior you
-- should include these in the hcLoadTimeSplices list in your HeistConfig.
defaultLoadTimeSplices :: MonadIO m => [(Text, (I.Splice m))]
defaultLoadTimeSplices =
    ("content", deprecatedContentCheck) -- To be removed in later versions
    : defaultInterpretedSplices


------------------------------------------------------------------------------
-- | The built-in set of static splices.  All the splices that used to be
-- enabled by default are included here.  To get the normal Heist behavior you
-- should include these in the hcLoadTimeSplices list in your HeistConfig.
defaultInterpretedSplices :: MonadIO m => [(Text, (I.Splice m))]
defaultInterpretedSplices =
    [ (applyTag, applyImpl)
    , (bindTag, bindImpl)
    , (ignoreTag, ignoreImpl)
    , (markdownTag, markdownSplice)
    ]


------------------------------------------------------------------------------
-- | Loads templates from disk.  This function returns just a template map so
-- you can load multiple directories and combine the maps before initializing
-- your HeistState.
loadTemplates :: FilePath -> EitherT [String] IO TemplateRepo
loadTemplates dir = do
    d <- lift $ readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
    case errs of
        [] -> right $ Map.fromList $ rights tlist
        _  -> left errs


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
emptyHS kg = HeistState Map.empty Map.empty Map.empty Map.empty
                        Map.empty True [] 0 [] Nothing kg False


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
    initHeist' keyGen hc


initHeist' :: Monad n
           => HE.KeyGen
           -> HeistConfig n
           -> EitherT [String] IO (HeistState n)
initHeist' keyGen (HeistConfig i lt c a rawTemplates) = do
    let empty = emptyHS keyGen
    tmap <- preproc keyGen lt rawTemplates
    let hs1 = empty { _spliceMap = Map.fromList i
                    , _templateMap = tmap
                    , _compiledSpliceMap = Map.fromList c
                    , _attrSpliceMap = Map.fromList a
                    }
    lift $ C.compileTemplates hs1


------------------------------------------------------------------------------
-- | Runs preprocess on a TemplateRepo and returns the modified templates.
preproc :: HE.KeyGen
        -> [(Text, I.Splice IO)]
        -> TemplateRepo
        -> EitherT [String] IO TemplateRepo
preproc keyGen splices templates = do
    let hs = (emptyHS keyGen) { _spliceMap = Map.fromList splices
                              , _templateMap = templates
                              , _preprocessingMode = True }
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
-- do configure the cache tag differently than how this function does it, you
-- will still probably want to pattern your approach after this function's
-- implementation.
initHeistWithCacheTag :: MonadIO n
                      => HeistConfig n
                      -> EitherT [String] IO (HeistState n, CacheTagState)
initHeistWithCacheTag (HeistConfig i lt c a rawTemplates) = do
    (ss, cts) <- liftIO mkCacheTag
    let tag = "cache"
    keyGen <- lift HE.newKeyGen

    -- We have to do one preprocessing pass with the cache setup splice.  This
    -- has to happen for both interpreted and compiled templates, so we do it
    -- here by itself because interpreted templates don't get the same load
    -- time splices as compiled templates.
    rawWithCache <- preproc keyGen [(tag, ss)] rawTemplates

    let hc' = HeistConfig ((tag, cacheImpl cts) : i) lt
                          ((tag, cacheImplCompiled cts) : c)
                          a rawWithCache
    hs <- initHeist' keyGen hc'
    return (hs, cts)

