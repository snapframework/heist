module Heist.Interpreted.Pandoc.Tests (tests) where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Map.Syntax                (( ## ))
import           Data.Monoid                    (mempty)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit                     as H
------------------------------------------------------------------------------
import           Heist
import           Heist.Interpreted.Internal
import           Heist.Splices.Markdown
import           Heist.TestCommon
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCase "heist/markdown"     markdownTest
        , testCase "heist/pandoc"       pandocTest
        , testCase "heist/pandoc_div"   pandocDivTest
        , testCase "heist/markdownText" markdownTextTest
        ]


------------------------------------------------------------------------------
-- | Markdown test on a file
markdownTest :: H.Assertion
markdownTest = renderTest "markdown" markdownHtmlExpected


------------------------------------------------------------------------------
-- | Markdown test on supplied text
markdownTextTest :: H.Assertion
markdownTextTest = do
    hs <- loadEmpty mempty mempty mempty mempty
    result <- evalHeistT markdownSplice
                         (X.TextNode "This *is* a test.")
                         hs
    H.assertEqual "Markdown text" markdownHtmlExpected
      (B.filter (/= '\n') $ toByteString $
        X.render (X.HtmlDocument X.UTF8 Nothing result))


-----------------------------------------------------------------------------
markdownHtmlExpected :: ByteString
markdownHtmlExpected =
    "<div class='markdown'><p>This <em>is</em> a test.</p></div>"


-----------------------------------------------------------------------------
-- | Pandoc test on a file
pandocTest :: H.Assertion
pandocTest = renderTest "pandoc" pandocNoDivHtmlExpected


pandocDivTest :: H.Assertion
pandocDivTest = renderTest "pandocdiv" pandocDivHtmlExpected


pandocNoDivHtmlExpected :: ByteString
pandocNoDivHtmlExpected = "<p>This <em>is</em> a test.</p>"


pandocDivHtmlExpected :: ByteString
pandocDivHtmlExpected =
  "<div class='foo test' id='pandoc'><p>This <em>is</em> a test.</p></div>"
  -- Implementaton dependent. Class is prepended in current implementation,
  -- it will be first attribute


pandocTestSplices :: Splices (Splice IO)
pandocTestSplices = do
    "pandocnodiv" ## pandocSplice optsNoDiv
    "pandocdiv"   ## pandocSplice optsDiv
  where
    optsNoDiv = setPandocWrapDiv Nothing defaultPandocOptions
    optsDiv = setPandocWrapDiv (Just "test") defaultPandocOptions


------------------------------------------------------------------------------
-- | Render a template and assert that it matches an expected result
renderTest  :: ByteString   -- ^ template name
            -> ByteString   -- ^ expected result
            -> H.Assertion
renderTest templateName expectedResult = do
    ets <- loadT "templates-pandoc" mempty pandocTestSplices mempty mempty
    let ts = either (error "Error loading templates") id ets
    Just (doc, _) <- renderTemplate ts templateName
    let result = B.filter (/= '\n') (toByteString doc)
    H.assertEqual ("Should match " ++ (show expectedResult)) expectedResult result

