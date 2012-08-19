{-# LANGUAGE BangPatterns #-}

module Heist where

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
import           Heist.Interpreted.Splices
import           Heist.Types


------------------------------------------------------------------------------
-- | The default set of static splices.  All the splices that used to be
-- enabled by default are included here.  You don't need to include them
-- anywhere else.
defaultStaticSplices :: MonadIO m => [(Text, (I.Splice m))]
defaultStaticSplices =
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
-- We don't provide functions to bind compiled splices because it doesn't make
-- any sense unless you re-compile all templates with the new splices.  If you
-- add any old-style runtime heist splices after calling this function, they
-- will still work fine.  If you add any templates later, then those templates
-- will be available for interpreted rendering, but not for compiled
-- rendering.
initHeist :: Monad n
          => [(Text, I.Splice n)]
          -- ^ Runtime splices
          -> [(Text, I.Splice IO)]
          -- ^ Static loadtime splices
          -> [(Text, C.Splice n)]
          -- ^ Dynamic loadtime splices
          -> HashMap TPath DocumentFile
          -> EitherT [String] IO (HeistState n IO)
initHeist rSplices sSplices dSplices rawTemplates = do
    keyGen <- lift HE.newKeyGen
    let empty = HeistState Map.empty Map.empty Map.empty Map.empty
                           True [] 0 return [] Nothing keyGen False
        hs0 = empty { _spliceMap = Map.fromList defaultStaticSplices
                                  `mappend` Map.fromList sSplices
                    , _templateMap = rawTemplates
                    , _preprocessingMode = True }
    tPairs <- lift $ evalHeistT
        (mapM preprocess $ Map.toList rawTemplates) (X.TextNode "") hs0
    let bad = lefts tPairs
        tmap = Map.fromList $ rights tPairs
        hs1 = empty { _spliceMap = Map.fromList rSplices
                    , _templateMap = tmap
                    , _compiledSpliceMap = Map.fromList dSplices
                    }

    if not (null bad)
      then left bad
      else lift $ C.compileTemplates hs1
--    when (not $ null bad) $ do
--        putStrLn "Errors in template processing:"
--        mapM_ (\e -> putStrLn $ "  " ++ show e) bad
--        error "exiting..."
    


------------------------------------------------------------------------------
-- | 
preprocess :: (TPath, DocumentFile)
           -> HeistT IO IO (Either String (TPath, DocumentFile))
preprocess (tpath, docFile) = do
    let tname = tpathName tpath
    !emres <- try $ I.evalTemplate tname
              :: HeistT IO IO (Either SomeException (Maybe Template))
    let f doc = (tpath, docFile { dfDoc = (dfDoc docFile)
                { X.docContent = doc } })
    return $! either (Left . show) (Right . maybe die f) emres
  where
    die = error "Preprocess didn't succeed!  This should never happen."


