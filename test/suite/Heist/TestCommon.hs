module Heist.TestCommon where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Text (Text)


------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import qualified Heist.Interpreted.Internal as I
import qualified Text.XmlHtml        as X


------------------------------------------------------------------------------
-- | The default doctype given to templates
doctype :: ByteString
doctype = B.concat
    [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
    , "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>" ]


loadT :: MonadIO m
      => FilePath
      -> [(Text, I.Splice m)]
      -> [(Text, I.Splice IO)]
      -> [(Text, C.Splice m)]
      -> [(Text, AttrSplice m)]
      -> IO (Either [String] (HeistState m))
loadT baseDir a b c d = runEitherT $ do
    ts <- loadTemplates baseDir
    let hc = HeistConfig (defaultInterpretedSplices ++ a)
                         (defaultLoadTimeSplices ++ b) c d ts
    initHeist hc


------------------------------------------------------------------------------
loadIO :: FilePath
       -> [(Text, I.Splice IO)]
       -> [(Text, I.Splice IO)]
       -> [(Text, C.Splice IO)]
       -> [(Text, AttrSplice IO)]
       -> IO (Either [String] (HeistState IO))
loadIO baseDir a b c d = runEitherT $ do
    ts <- loadTemplates baseDir
    let hc = HeistConfig (defaultInterpretedSplices ++ a)
                         (defaultLoadTimeSplices ++ b) c d ts
    initHeist hc


------------------------------------------------------------------------------
loadHS :: FilePath -> IO (HeistState IO)
loadHS baseDir = do
    etm <- runEitherT $ do
        templates <- loadTemplates baseDir
        let hc = HeistConfig defaultInterpretedSplices
                             defaultLoadTimeSplices [] [] templates
        initHeist hc
    either (error . concat) return etm


loadEmpty :: [(Text, I.Splice IO)]
          -> [(Text, I.Splice IO)]
          -> [(Text, C.Splice IO)]
          -> [(Text, AttrSplice IO)]
          -> IO (HeistState IO)
loadEmpty a b c d = do
    let hc = HeistConfig (defaultInterpretedSplices ++ a)
                         (defaultLoadTimeSplices ++ b) c d Map.empty
    res <- runEitherT $ initHeist hc
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

