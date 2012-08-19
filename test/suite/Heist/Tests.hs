module Heist.Tests
  ( tests
  , loadT
  , loadTS
  , loadEmpty
  , testTemplate
  , testTemplateEval
  , quickRender
  , isLeft
  ) where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H


------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled.Internal as C
import           Heist.Interpreted.Internal
import           Heist.Types
import qualified Text.XmlHtml        as X


tests :: [Test]
tests = [ testCase     "loadErrors"           loadErrorsTest
        ]


------------------------------------------------------------------------------
loadT :: FilePath
      -> [(Text, Splice IO)]
      -> [(Text, Splice IO)]
      -> [(Text, C.Splice IO)]
      -> IO (Either [String] (HeistState IO IO))
loadT baseDir r s d = runEitherT $ do
    ts <- loadTemplates baseDir
    initHeist r s d ts


------------------------------------------------------------------------------
loadTS :: FilePath -> IO (HeistState IO IO)
loadTS baseDir = do
    etm <- runEitherT $
        loadTemplates baseDir >>= initHeist [] [] []
    either (error . concat) return etm


loadEmpty :: [(Text, Splice IO)]
          -> [(Text, Splice IO)]
          -> [(Text, C.Splice IO)]
          -> IO (HeistState IO IO)
loadEmpty a b c = do
    res <- runEitherT $ initHeist a b c Map.empty
    either (error . concat) return res


testTemplate :: FilePath -> ByteString -> IO ByteString
testTemplate tdir tname = do
    ts <- loadTS tdir
    Just (resDoc, _) <- renderTemplate ts tname
    return $ toByteString resDoc


testTemplateEval :: ByteString -> IO (Maybe Template)
testTemplateEval tname = do
    ts <- loadTS "templates"
    evalHeistT (evalWithHooks tname) (X.TextNode "") ts


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.  (Old convenience code.)
quickRender :: FilePath -> ByteString -> IO (Maybe ByteString)
quickRender baseDir name = do
    ts  <- loadTS baseDir
    res <- renderTemplate ts name
    return (fmap (toByteString . fst) res)


------------------------------------------------------------------------------
-- | Tests that load fails correctly on errors.
loadErrorsTest :: H.Assertion
loadErrorsTest = do
    ets <- loadT "templates-bad" [] [] []
    either (H.assertEqual "load errors test" expected)
           (const $ H.assertFailure "No failure when loading templates-bad")
           ets
  where
    expected =
        ["templates-bad/etc.tpl: template recursion exceeded max depth, you probably have infinite splice recursion!"
        ,"templates-bad/ioc.tpl: must supply \"template\" attribute in <apply>"
        ,"templates-bad/noroot.tpl: must supply \"template\" attribute in <apply>"
        ]


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


