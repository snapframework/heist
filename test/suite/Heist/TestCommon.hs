module Heist.TestCommon where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Error
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)


------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled.Internal as C
import           Heist.Interpreted.Internal
import qualified Text.XmlHtml        as X


------------------------------------------------------------------------------
-- | The default doctype given to templates
doctype :: ByteString
doctype = B.concat
    [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
    , "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>" ]


loadT :: Monad m
      => FilePath
      -> [(Text, Splice m)]
      -> [(Text, Splice IO)]
      -> [(Text, C.Splice m)]
      -> [(Text, AttrSplice m)]
      -> IO (Either [String] (HeistState m))
loadT baseDir r s d a = runEitherT $ do
    ts <- loadTemplates baseDir
    initHeist r s d a ts


------------------------------------------------------------------------------
loadIO :: FilePath
       -> [(Text, Splice IO)]
       -> [(Text, Splice IO)]
       -> [(Text, C.Splice IO)]
       -> [(Text, AttrSplice IO)]
       -> IO (Either [String] (HeistState IO))
loadIO baseDir r s d a = runEitherT $ do
    ts <- loadTemplates baseDir
    initHeist r s d a ts


------------------------------------------------------------------------------
loadHS :: FilePath -> IO (HeistState IO)
loadHS baseDir = do
    etm <- runEitherT $
        loadTemplates baseDir >>= initHeist [] [] [] []
    either (error . concat) return etm


loadEmpty :: [(Text, Splice IO)]
          -> [(Text, Splice IO)]
          -> [(Text, C.Splice IO)]
          -> [(Text, AttrSplice IO)]
          -> IO (HeistState IO)
loadEmpty a b c d = do
    res <- runEitherT $ initHeist a b c d Map.empty
    either (error . concat) return res


testTemplate :: FilePath -> ByteString -> IO ByteString
testTemplate tdir tname = do
    ts <- loadHS tdir
    Just (resDoc, _) <- renderTemplate ts tname
    return $ toByteString resDoc


testTemplateEval :: ByteString -> IO (Maybe Template)
testTemplateEval tname = do
    ts <- loadHS "templates"
    md <- evalHeistT (evalWithDoctypes tname) (X.TextNode "") ts
    return $ fmap X.docContent md


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.  (Old convenience code.)
quickRender :: FilePath -> ByteString -> IO (Maybe ByteString)
quickRender baseDir name = do
    ts  <- loadHS baseDir
    res <- renderTemplate ts name
    return (fmap (toByteString . fst) res)


