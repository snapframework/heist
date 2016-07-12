module Heist.TestCommon where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Monad.Trans
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Monoid


------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled             as C
import           Heist.Internal.Types
import qualified Heist.Interpreted          as I
import qualified Heist.Interpreted.Internal as I
import qualified Text.XmlHtml               as X


------------------------------------------------------------------------------
-- | The default doctype given to templates
doctype :: ByteString
doctype = B.concat
    [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
    , "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>" ]


loadT :: MonadIO m
      => FilePath
      -> Splices (I.Splice m)
      -> Splices (I.Splice IO)
      -> Splices (C.Splice m)
      -> Splices (AttrSplice m)
      -> IO (Either [String] (HeistState m))
loadT baseDir a b c d = runExceptT $ do
    let sc = SpliceConfig (defaultInterpretedSplices `mappend` a)
                          (defaultLoadTimeSplices `mappend` b) c d
                          [loadTemplates baseDir] (const True)
    ExceptT $ initHeist $ HeistConfig sc "" False


------------------------------------------------------------------------------
loadIO :: FilePath
       -> Splices (I.Splice IO)
       -> Splices (I.Splice IO)
       -> Splices (C.Splice IO)
       -> Splices (AttrSplice IO)
       -> IO (Either [String] (HeistState IO))
loadIO baseDir a b c d = runExceptT $ do
    let sc = SpliceConfig (defaultInterpretedSplices >> a)
                          (defaultLoadTimeSplices >> b) c d
                          [loadTemplates baseDir] (const True)
    ExceptT $ initHeist $ HeistConfig sc "" False


------------------------------------------------------------------------------
loadHS :: FilePath -> IO (HeistState IO)
loadHS baseDir = do
    etm <- runExceptT $ do
        let sc = SpliceConfig defaultInterpretedSplices
                              defaultLoadTimeSplices mempty mempty
                              [loadTemplates baseDir] (const True)
        ExceptT $ initHeist $ HeistConfig sc "" False
    either (error . concat) return etm


loadEmpty :: Splices (I.Splice IO)
          -> Splices (I.Splice IO)
          -> Splices (C.Splice IO)
          -> Splices (AttrSplice IO)
          -> IO (HeistState IO)
loadEmpty a b c d = do
    let sc = SpliceConfig (defaultInterpretedSplices `mappend` a)
                          (defaultLoadTimeSplices `mappend` b) c d mempty
                          (const True)
    res <- initHeist $ HeistConfig sc "" False
    either (error . concat) return res


testTemplate :: FilePath -> ByteString -> IO ByteString
testTemplate tdir tname = do
    ts <- loadHS tdir
    Just (resDoc, _) <- I.renderTemplate ts tname
    return $ toByteString resDoc


testTemplateEval :: ByteString -> IO (Maybe Template)
testTemplateEval tname = do
    ts <- loadHS "templates"
    md <- evalHeistT (I.evalWithDoctypes tname) (X.TextNode "") ts
    return $ fmap X.docContent md


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.  (Old convenience code.)
quickRender :: FilePath -> ByteString -> IO (Maybe ByteString)
quickRender baseDir name = do
    ts  <- loadHS baseDir
    res <- I.renderTemplate ts name
    return (fmap (toByteString . fst) res)

cRender :: HeistState IO -> ByteString -> IO ByteString
cRender hs name = do
    builder <- fst $ fromJust $ C.renderTemplate hs name
    return $ toByteString builder

iRender :: HeistState IO -> ByteString -> IO ByteString
iRender hs name = do
    builder <- I.renderTemplate hs name
    return $ toByteString $ fst $ fromJust builder

