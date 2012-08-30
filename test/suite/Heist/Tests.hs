module Heist.Tests
  ( tests
  ) where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Text (Text)
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H


------------------------------------------------------------------------------
import           Heist.TestCommon
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I


tests :: [Test]
tests = [ testCase     "loadErrors"            loadErrorsTest
        , testCase     "attrsplice/autocheck"  attrSpliceTest
        ]


------------------------------------------------------------------------------
-- | Tests that load fails correctly on errors.
loadErrorsTest :: H.Assertion
loadErrorsTest = do
    ets <- loadIO "templates-bad" [] [] [] []
    either (H.assertEqual "load errors test" expected)
           (const $ H.assertFailure "No failure when loading templates-bad")
           ets
  where
    expected =
        ["templates-bad/bind-infinite-loop.tpl: template recursion exceeded max depth, you probably have infinite splice recursion!"
        ,"templates-bad/apply-missing-attr.tpl: must supply \"template\" attribute in <apply>"
        ,"templates-bad/bind-missing-attr.tpl: must supply \"tag\" attribute in <bind>"
        ,"templates-bad/apply-template-not-found.tpl: apply tag cannot find template \"/page\""
        ]


attrSplice :: Text -> StateT Text IO [(Text, Text)]
attrSplice v = do
    val <- get
    let checked = if v == val
                    then [("checked","")]
                    else []
    return $ ("name", v) : checked

attrSpliceTest :: IO ()
attrSpliceTest = do
    ehs <- loadT "templates" [] [] [] [("autocheck", attrSplice)]
    let hs = either (error . show) id ehs
        runtime = fromJust $ C.renderCompiledTemplate "attr_splice" hs

    mres <- evalStateT (I.renderTemplate hs "attr_splice") "foo"
    H.assertEqual "interpreted foo" expected1
      (toByteString $ fst $ fromJust mres)
    mres2 <- evalStateT (I.renderTemplate hs "attr_splice") "bar"
    H.assertEqual "interpreted bar" expected2
      (toByteString $ fst $ fromJust mres2)

    builder <- evalStateT (fst runtime) "foo"
    H.assertEqual "compiled foo" expected1
      (toByteString builder)
    builder2 <- evalStateT (fst runtime) "bar"
    H.assertEqual "compiled bar" expected2
      (toByteString builder2)
  where
    expected1 = doctype `B.append` "\n<input type='checkbox' name='foo' checked />\n<input type='checkbox' name='bar' />\n"
    expected2 = doctype `B.append` "\n<input type='checkbox' name='foo' />\n<input type='checkbox' name='bar' checked />\n"


