{-# LANGUAGE BangPatterns #-}

{-|

This module defines the core data types used by Heist.  In practice you will
also want to import one or both of 'Heist.Compiled' or 'Heist.Interpreted' to
get functions needed for writing splices.

-}

module Heist
  ( Template
  , TPath
  , HeistConfig(..)
  , defaultLoadTimeSplices
  , loadTemplates
  , initHeist
  , initHeistWithCacheTag
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
  ) where

import           Control.Error
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import           Control.Monad.Trans
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
    , hcTemplates          :: HashMap TPath DocumentFile
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
    [ (applyTag, applyImpl)
    , (bindTag, bindImpl)
    , (ignoreTag, ignoreImpl)
    , (markdownTag, markdownSplice)
    ]


------------------------------------------------------------------------------
-- | Loads templates from disk.  This function returns just a template map so
-- you can load multiple directories and combine the maps before initializing
-- your HeistState.
loadTemplates :: FilePath -> EitherT [String] IO (HashMap TPath DocumentFile)
loadTemplates dir = do
    d <- lift $ readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
    case errs of
        [] -> right $ Map.fromList $ rights tlist
        _  -> left errs


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
initHeist (HeistConfig i lt c a rawTemplates) = do
    keyGen <- lift HE.newKeyGen
    let empty = HeistState Map.empty Map.empty Map.empty Map.empty
                           Map.empty True [] 0 [] Nothing keyGen False
        hs0 = empty { _spliceMap = Map.fromList lt
                    , _templateMap = rawTemplates
                    , _preprocessingMode = True }
    tPairs <- lift $ evalHeistT
        (mapM preprocess $ Map.toList rawTemplates) (X.TextNode "") hs0
    let bad = lefts tPairs
        tmap = Map.fromList $ rights tPairs
        hs1 = empty { _spliceMap = Map.fromList i
                    , _templateMap = tmap
                    , _compiledSpliceMap = Map.fromList c
                    , _attrSpliceMap = Map.fromList a
                    }

    if not (null bad)
      then left bad
      else lift $ C.compileTemplates hs1


------------------------------------------------------------------------------
-- | 
preprocess :: (TPath, DocumentFile)
           -> HeistT IO IO (Either String (TPath, DocumentFile))
preprocess (tpath, docFile) = do
    let tname = tpathName tpath
    !emdoc <- try $ I.evalWithDoctypes tname
              :: HeistT IO IO (Either SomeException (Maybe X.Document))
    let f doc = (tpath, docFile { dfDoc = doc })
    return $! either (Left . show) (Right . maybe die f) emdoc
  where
    die = error "Preprocess didn't succeed!  This should never happen."


------------------------------------------------------------------------------
-- | This function is the easiest way to set up your HeistState with a cache
-- tag.  It sets up all the necessary splices properly.  If you need to do
-- configure the cache tag differently than how this function does it, you
-- will still probably want to pattern your approach after this function's
-- implementation.
initHeistWithCacheTag :: MonadIO n
                      => HeistConfig n
                      -> EitherT [String] IO (HeistState n, CacheTagState)
initHeistWithCacheTag (HeistConfig i lt c a rawTemplates) = do
    (ss, cts) <- liftIO mkCacheTag
    let tag = "cache"
        hc' = HeistConfig ((tag, cacheImpl cts) : i)
                          ((tag, ss) : lt)
                          ((tag, cacheImplCompiled cts) : c)
                          a rawTemplates
    hs <- initHeist hc'
    return (hs, cts)

