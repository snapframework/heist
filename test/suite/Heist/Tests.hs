{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module Heist.Tests
  ( tests
  ) where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad.State
import qualified Data.ByteString.Char8           as B
import           Data.List
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                       as T
import           Test.Framework                  (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit                      as H


------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled                  as C
import           Heist.Internal.Types
import qualified Heist.Interpreted               as I
import           Heist.Splices.Cache
import           Heist.Splices.Html
import           Heist.TemplateDirectory
import           Heist.Tutorial.AttributeSplices
import           Heist.Tutorial.CompiledSplices

import           Heist.TestCommon

tests :: [Test]
tests = [ testCase     "loadErrors"            loadErrorsTest
        , testCase     "attrsplice/autocheck"  attrSpliceTest
        , testCase     "tdirCache"             tdirCacheTest
        , testCase     "headMerge"             headMergeTest
        , testCase     "bindApplyInteraction"  bindApplyInteractionTest
        , testCase     "backslashHandling"     backslashHandlingTest
        ]


------------------------------------------------------------------------------
-- | Tests that load fails correctly on errors.
loadErrorsTest :: H.Assertion
loadErrorsTest = do
    ets <- loadIO "templates-bad" mempty mempty mempty mempty
    either (H.assertEqual "load errors test" expected . sort)
           (const $ H.assertFailure "No failure when loading templates-bad")
           ets
  where
    expected = sort
#if MIN_VERSION_base(4,9,0)
        ["templates-bad/apply-missing-attr.tpl: must supply \"template\" attribute in <apply>\nCallStack (from HasCallStack):\n  error, called at src/Heist/Common.hs:76:15 in main:Heist.Common"
        ,"templates-bad/apply-template-not-found.tpl: apply tag cannot find template \"/page\"\nCallStack (from HasCallStack):\n  error, called at src/Heist/Common.hs:76:15 in main:Heist.Common"
        ,"templates-bad/bind-infinite-loop.tpl: template recursion exceeded max depth, you probably have infinite splice recursion!\nCallStack (from HasCallStack):\n  error, called at src/Heist/Common.hs:76:15 in main:Heist.Common"
        ,"templates-bad/bind-missing-attr.tpl: must supply \"tag\" attribute in <bind>\nCallStack (from HasCallStack):\n  error, called at src/Heist/Common.hs:76:15 in main:Heist.Common"
        ]
#else
        ["templates-bad/bind-infinite-loop.tpl: template recursion exceeded max depth, you probably have infinite splice recursion!"
        ,"templates-bad/apply-template-not-found.tpl: apply tag cannot find template \"/page\""
        ,"templates-bad/bind-missing-attr.tpl: must supply \"tag\" attribute in <bind>"
        ,"templates-bad/apply-missing-attr.tpl: must supply \"template\" attribute in <apply>"
        ]
#endif


attrSpliceTest :: IO ()
attrSpliceTest = do
    ehs <- loadT "templates" mempty mempty mempty
                 ("autocheck" ## lift . autocheckedSplice)
    let hs = either (error . show) id ehs
        runtime = fromJust $ C.renderTemplate hs "attr_splice"

    mres <- evalStateT (I.renderTemplate hs "attr_splice") "foo"
    H.assertEqual "interpreted foo" expected1
      (toByteString $ fst $ fromJust mres)
    mres2 <- evalStateT (I.renderTemplate hs "attr_splice") "bar"
    H.assertEqual "interpreted bar" expected2
      (toByteString $ fst $ fromJust mres2)

    builder <- evalStateT (fst runtime) "foo"
    H.assertEqual "compiled foo" expected3
      (toByteString builder)
    builder2 <- evalStateT (fst runtime) "bar"
    H.assertEqual "compiled bar" expected4
      (toByteString builder2)
  where
    expected1 = "<input type='checkbox' value='foo' checked />\n<input type='checkbox' value='bar' />\n"
    expected2 = "<input type='checkbox' value='foo' />\n<input type='checkbox' value='bar' checked />\n"
    expected3 = "<input type=\"checkbox\" value=\"foo\" checked />\n<input type=\"checkbox\" value=\"bar\" />\n"
    expected4 = "<input type=\"checkbox\" value=\"foo\" />\n<input type=\"checkbox\" value=\"bar\" checked />\n"

fooSplice :: I.Splice (StateT Int IO)
fooSplice = do
    val <- get
    put val
    I.textSplice $ T.pack $ show val

tdirCacheTest :: IO ()
tdirCacheTest = do
    let rSplices = ("foosplice" ## fooSplice)
        dSplices = ("foosplice" ## stateSplice)
        sc = SpliceConfig rSplices mempty dSplices mempty mempty (const True)
        hc = HeistConfig sc "" False
    td <- newTemplateDirectory' "templates" hc

    [a,b,c,d] <- evalStateT (testInterpreted td) 5
    H.assertBool "interpreted doesn't cache" $ a == b
    H.assertBool "interpreted doesn't clear" $ b /= c
    H.assertBool "interpreted doesn't reload" $ c /= d

    td' <- newTemplateDirectory' "templates" hc
    [e,f,g,h] <- evalStateT (testCompiled td') 5
    H.assertBool "compiled doesn't cache" $ e == f
    H.assertBool "compiled doesn't clear" $ f /= g
    H.assertBool "compiled doesn't reload" $ g /= h
  where
    testInterpreted td = do
        hs <- liftIO $ getDirectoryHS td
        cts <- liftIO $ getDirectoryCTS td
        a <- I.renderTemplate hs "cache"
        modify (+1)
        b <- I.renderTemplate hs "cache"
        liftIO $ clearCacheTagState cts
        c <- I.renderTemplate hs "cache"
        modify (+1)
        _ <- liftIO $ reloadTemplateDirectory td

        -- The reload changes the HeistState, so we have to get it again
        hs' <- liftIO $ getDirectoryHS td
        d <- I.renderTemplate hs' "cache"
        return $ map (toByteString . fst . fromJust) [a,b,c,d]

    testCompiled td = do
        hs <- liftIO $ getDirectoryHS td
        cts <- liftIO $ getDirectoryCTS td
        a <- fst $ fromJust $ C.renderTemplate hs "cache"
        modify (+1)
        b <- fst $ fromJust $ C.renderTemplate hs "cache"
        liftIO $ clearCacheTagState cts
        c <- fst $ fromJust $ C.renderTemplate hs "cache"
        modify (+1)
        _ <- liftIO $ reloadTemplateDirectory td

        -- The reload changes the HeistState, so we have to get it again
        hs' <- liftIO $ getDirectoryHS td
        d <- fst $ fromJust $ C.renderTemplate hs' "cache"
        return $ map toByteString [a,b,c,d]


headMergeTest :: IO ()
headMergeTest = do
    ehs <- loadT "templates" mempty (htmlTag ## htmlImpl) mempty mempty
    let hs = either (error . show) id ehs
        runtime = fromJust $ C.renderTemplate hs "head_merge/index"
    mres <- fst runtime
    H.assertEqual "assertion failed" expected
      (toByteString mres)
  where
    expected = B.intercalate "\n"
      ["<html><head>\n<link href='wrapper-link' />\n"
      ,"<link href='nav-link' />\n\n<link href='index-link' />"
      ,"</head>\n\n<body>\n\n<div>nav bar</div>\n\n\n"
      ,"<div>index page</div>\n\n</body>\n</html>\n\n"
      ]

bindApplyInteractionTest :: IO ()
bindApplyInteractionTest = do
    hs <- loadHS "templates"

    cOut <- cRender hs "bind-apply-interaction/caller"
    H.assertEqual "compiled failure" cExpected cOut

    iOut <- iRender hs "bind-apply-interaction/caller"
    H.assertEqual "interpreted failure" iExpected iOut
  where
    cExpected = B.intercalate "\n"
      ["\nThis is a test."
      ,"===bind content===\nAnother test line."
      ,"apply content\nLast test line."
      ,"\n"
      ]
    iExpected = B.unlines
      ["\nThis is a test."
      ,"===bind content==="
      ,"Another test line."
      ,"apply content"
      ,"Last test line."
      ,""
      ]


------------------------------------------------------------------------------
-- | Test backslash escaping in the attribute parser.
backslashHandlingTest :: IO ()
backslashHandlingTest = do
    hs <- loadHS "templates"
    cOut <- cRender hs "backslash"
    H.assertEqual "compiled failure" expected cOut

    iOut <- iRender hs "backslash"
    H.assertEqual "interpreted failure" expected iOut
  where
    expected = "<foo regex='abc\\d+\\\\e'></foo>\n"

