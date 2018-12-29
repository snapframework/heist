{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heist.Compiled.Tests where

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Exception
import           Control.Monad.Trans.Except
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char
import           Data.IORef
import           Data.Maybe
import           Data.Map.Syntax
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H
import qualified Text.XmlHtml as X


------------------------------------------------------------------------------
import           Heist
import           Heist.Compiled
import           Heist.Compiled.Internal
import           Heist.Internal.Types
import           Heist.Tutorial.CompiledSplices
import           Heist.TestCommon

-- NOTE: We can't test compiled templates on the templates directory as it
-- stands today because that directory contains some error conditions such as
-- infinite bind loops, apply tags with no template attribute, and apply tags
-- with ".." in the tag path (which doesn't currently work).

tests :: [Test]
tests = [ testCase     "compiled/simple"        simpleCompiledTest
        , testCase     "compiled/people"        peopleTest
        , testCase     "compiled/namespace1"    namespaceTest1
        , testCase     "compiled/namespace2"    namespaceTest2
        , testCase     "compiled/namespace3"    namespaceTest3
        , testCase     "compiled/namespace4"    namespaceTest4
        , testCase     "compiled/namespace5"    namespaceTest5
        , testCase     "compiled/no-ns-splices" noNsSplices
        , testCase     "compiled/ns-nested"     nsNestedUnused
        , testCase     "compiled/nsbind"        nsBindTest
        , testCase     "compiled/nsbinderr"     nsBindErrorTest
        , testCase     "compiled/nscall"        nsCallTest
        , testCase     "compiled/nscallerr"     nsCallErrTest
        , testCase     "compiled/nsbindstack"   nsBindStackTest
        , testCase     "compiled/doctype"       doctypeTest
        , testCase     "compiled/exceptions"    exceptionsTest
        , testCase     "compiled/defer"         deferTest
        , testCase     "compiled/readert"       readerTTest
        ]

simpleCompiledTest :: IO ()
simpleCompiledTest = do
    res <- runWithStateSplice "templates"
    H.assertEqual "compiled state splice" expected res
  where
    expected =
      mappend doctype "\n\n<html>\n3\n</html>\n"

peopleTest :: IO ()
peopleTest = do
    res <- personListTest "templates"
    H.assertEqual "people splice" expected res
  where
    expected =
      "\n<p>Doe, John: 42&#32;years old</p>\n\n<p>Smith, Jane: 21&#32;years old</p>\n\n"

templateHC :: HeistConfig IO IO
templateHC = HeistConfig sc "" False
  where
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ ("foo" ## return (yieldPureText "aoeu"))
                & scTemplateLocations .~ [loadTemplates "templates"]

genericTest :: String -> ByteString -> ByteString -> IO ()
genericTest nm template expected = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist templateHC
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs template
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual nm (Right expected) res

doctypeTest :: IO ()
doctypeTest = genericTest "doctype test" "rss" expected
  where
    expected = encodeUtf8
      "<rss><channel><link>http://www.devalot.com/</link></channel></rss>\n"

namespaceTest1 :: IO ()
namespaceTest1 = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist templateHC
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test 1" (Right expected) res
  where
    expected = "Alpha\naoeu\nBeta\n<h:foo aoeu='htns'>Inside h:foo</h:foo>\nEnd\n"


namespaceTest2 :: IO ()
namespaceTest2 = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ templateHC & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test 2" (Right expected) res
  where
    expected = "Alpha\naoeu\nBeta\n<h:foo aoeu='htns'>Inside h:foo</h:foo>\nEnd\n"


namespaceTest3 :: IO ()
namespaceTest3 = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ templateHC & hcNamespace .~ "h"
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test 3" (Right expected) res
  where
    expected = "Alpha\n<foo aoeu='htns'>Inside foo</foo>\nBeta\naoeu\nEnd\n"


namespaceTest4 :: IO ()
namespaceTest4 = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ templateHC & hcNamespace .~ "h"
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test 4" (Right expected) res
  where
    expected = "Alpha\n<foo aoeu='htns'>Inside foo</foo>\nBeta\naoeu\nEnd\n"


namespaceTest5 :: IO ()
namespaceTest5 = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ templateHC & hcNamespace .~ "h"
                                     & hcCompiledSplices .~ mempty
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test 5" (Left ["templates/namespaces.tpl: No splice bound for h:foo"]) res

------------------------------------------------------------------------------
-- | The templates-no-ns directory should have no tags beginning with h: so
-- this test will throw an error.
noNsSplices :: IO ()
noNsSplices = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist hc
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "test"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "noNsSplices" (Left [noNamespaceSplicesMsg "h:"]) res
  where
    hc = HeistConfig sc "h" True
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ ("foo" ## return (yieldPureText "aoeu"))
                & scTemplateLocations .~ [loadTemplates "templates-no-ns"]


------------------------------------------------------------------------------
-- | Test that no namespace splice message works correctly when there are no
-- top level splices used
nsNestedUnused :: IO ()
nsNestedUnused = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist hc
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "test"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "ns nested unused warn test" (Right "<div>aeou</div>\n") res
  where
    hc = HeistConfig sc "h" False
    sc = mempty & scCompiledSplices .~ ("foo" ## return $ yieldPureText "aeou")
                & scTemplateLocations .~ [loadTemplates "templates-ns-nested"]


nsBindTemplateHC :: String -> HeistConfig IO IO
nsBindTemplateHC dir = HeistConfig sc "h" False
  where
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ nsBindTestSplices
                & scTemplateLocations .~ [loadTemplates dir]


nsBindTestSplices :: Splices (Splice IO)
nsBindTestSplices = do
    "call" ## do
        tpl <- withSplices (callTemplate "_call")
               nsBindSubSplices (return ())
        return $ yieldRuntime $ codeGen tpl
    "main" ## nsBindSubImpl (return ())
    "main2" ## nsBindSubImpl (return ())


nsBindSubImpl :: RuntimeSplice IO b -> Splice IO
nsBindSubImpl _ = do
    tpl <- withSplices runChildren nsBindSubSplices (return ())
    return $ yieldRuntime $ codeGen tpl


nsBindSubSplices :: Splices (RuntimeSplice IO () -> Splice IO)
nsBindSubSplices = do
    "sub" ## pureSplice . textSplice $ const "asdf"
    "recurse" ## nsBindSubImpl


nsBindTest :: IO ()
nsBindTest = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ (nsBindTemplateHC "templates-nsbind")
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nsbind"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace bind test" (Right expected)  res
  where
    expected = "Alpha\n\nBeta\nasdf\nGamma\n<sub></sub>\n\n"


------------------------------------------------------------------------------
-- | Test splice error reporting.
nsBindErrorTest :: IO ()
nsBindErrorTest = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ (nsBindTemplateHC "templates-nsbind")
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nsbinderror"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace bind error test" (Left [ err1, err2, err3 ])  res
  where
    err1 = "templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid3\n   ... via templates-nsbind/nsbinderror.tpl: h:main2\nBound splices: h:sub h:recurse h:call h:main2 h:main\nNode: Element {elementTag = \"h:invalid3\", elementAttrs = [], elementChildren = []}"
    err2 = "templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid2\n   ... via templates-nsbind/nsbinderror.tpl: h:recurse\n   ... via templates-nsbind/nsbinderror.tpl: h:main\nBound splices: h:sub h:recurse h:call h:main2 h:main\nNode: Element {elementTag = \"h:invalid2\", elementAttrs = [], elementChildren = []}"
    err3 = "templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid1\nBound splices: h:call h:main2 h:main\nNode: Element {elementTag = \"h:invalid1\", elementAttrs = [], elementChildren = []}"


------------------------------------------------------------------------------
-- | Test splice error data structure.
nsBindStackTest :: IO ()
nsBindStackTest = do
    res <- initHeist (nsBindTemplateHC "templates-nsbind") >>=
           return . (either Left (Right . _spliceErrors))

    H.assertEqual "namespace bind stack test" (Right [ err1, err2, err3 ]) res
  where
    err1 = SpliceError [ ( ["nsbinderror"]
                         , Just "templates-nsbind/nsbinderror.tpl"
                         , "h:main2") ]
               (Just "templates-nsbind/nsbinderror.tpl")
               ["h:sub","h:recurse","h:call","h:main2","h:main"]
               (X.Element "h:invalid3" [] [])
               "No splice bound for h:invalid3"
    err2 = SpliceError [ ( ["nsbinderror"]
                         , Just "templates-nsbind/nsbinderror.tpl"
                         , "h:recurse")
                       , ( ["nsbinderror"]
                         , Just "templates-nsbind/nsbinderror.tpl"
                         ,"h:main") ]
               (Just "templates-nsbind/nsbinderror.tpl")
               ["h:sub","h:recurse","h:call","h:main2","h:main"]
               (X.Element "h:invalid2" [] [])
               "No splice bound for h:invalid2"
    err3 = SpliceError []
               (Just "templates-nsbind/nsbinderror.tpl")
               ["h:call","h:main2","h:main"]
               (X.Element "h:invalid1" [] [])
               "No splice bound for h:invalid1"


nsCallTest :: IO ()
nsCallTest = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ (nsBindTemplateHC "templates-nscall")
                                     & hcErrorNotBound .~ True
                                     & hcCompiledTemplateFilter .~ nsFilter
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nscall"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace call test" (Right "Top\n\nInside 1\nCalled\nasdf\n\nInside 2\n\n") res
  where
    nsFilter = (/=) (fromIntegral $ ord '_') . B.head . head


nsCallErrTest :: IO ()
nsCallErrTest = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ (nsBindTemplateHC "templates-nscall")
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nscall"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace call error test"
      (Left $ Set.fromList [ err1, err2 ])
      (first Set.fromList res)
  where
    err1 = "templates-nscall/_call.tpl: No splice bound for h:sub\nBound splices: h:call h:main2 h:main\nNode: Element {elementTag = \"h:sub\", elementAttrs = [], elementChildren = []}"
    err2 = "templates-nscall/_invalid.tpl: No splice bound for h:invalid\nBound splices: h:call h:main2 h:main\nNode: Element {elementTag = \"h:invalid\", elementAttrs = [], elementChildren = []}"


------------------------------------------------------------------------------
-- | Test exception handling in template load.
exceptionsTest :: IO ()
exceptionsTest = do
    res <- Control.Exception.catch
             (runExceptT $ do
                  hs <- ExceptT $ initHeist hc
                  -- The rest needed only for type inference.
                  runner <- noteT ["Error rendering"] $ hoistMaybe $
                              renderTemplate hs ""
                  _ <- lift $ fst runner
                  throwE ["Unexpected success"])
             (\(e :: CompileException) -> return $ Right
                                            (show e, head $
                                                       exceptionContext e))

    H.assertEqual "exceptions" (Right (msg, err)) $ res

  where
    msg = "templates-loaderror/_error.tpl: Exception in splice compile: Prelude.read: no parse\n   ... via templates-loaderror/_error.tpl: h:adder\n   ... via templates-loaderror/test.tpl: h:call2\nBound splices: h:adder h:call2 h:call1\nNode: Element {elementTag = \"h:adder\", elementAttrs = [(\"value\",\"noparse\")], elementChildren = []}"
    err = SpliceError [ ( ["test"]
                        , Just "templates-loaderror/_error.tpl"
                        , "h:adder"),
                        ( ["test"]
                        , Just "templates-loaderror/test.tpl"
                        ,"h:call2") ]
              (Just "templates-loaderror/_error.tpl")
              ["h:adder", "h:call2", "h:call1"]
              (X.Element "h:adder" [("value", "noparse")] [])
              "Exception in splice compile: Prelude.read: no parse"
    hc = HeistConfig sc "h" True
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ splices
                & scTemplateLocations .~ [loadTemplates "templates-loaderror"]
    splices = do
        "call1" ## callTemplate "_ok"
        "call2" ## callTemplate "_error"
        "adder" ## do
            value :: Int <- read . T.unpack . fromJust .
                              X.getAttribute "value" <$> getParamNode
            return $ yieldPureText $ T.pack $ show $ 1 + value


------------------------------------------------------------------------------
-- | Test for defer functions to see that they correctly save the result of
-- a runtime computation.
deferTest :: IO ()
deferTest = do
    rs <- mapM newIORef $ replicate 5 (0 :: Int)
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ hc rs
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "test"
        b <- lift $ fst runner
        return $ toByteString b

    vs <- mapM readIORef rs
    H.assertEqual "defer test" ([2, 1, 1, 1, 1], Right msg) (vs, res)
  where
    msg = "1&#32;2\n1&#32;1\n1&#32;1\n\n1&#32;1\n"
    hc rs = HeistConfig (sc rs) "h" True
    sc rs = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                   & scCompiledSplices .~ (splices rs)
                   & scTemplateLocations .~ [loadTemplates "templates-defer"]
    splices [r1, r2, r3, r4, r5] = do
        "plain" ## subSplice $ addAndReturn r1
        "defer" ## deferMap return subSplice $ addAndReturn r2
        "maydefer" ## mayDeferMap (return . Just) subSplice $ addAndReturn r3
        "maydefer2" ## mayDeferMap (const $ return Nothing) subSplice $
                         addAndReturn r4
        "defermany" ## deferMany subSplice $ addAndReturn' r5
    subSplice =
        withSplices runChildren
          ("use" ## \n -> return $ yieldRuntimeText $ return . T.pack . show =<< n)
    addAndReturn r = liftIO $ modifyIORef r (+1) >> readIORef r
    addAndReturn' r = liftIO $ do
        modifyIORef r (+1)
        val <- readIORef r
        return [val]


------------------------------------------------------------------------------
-- | Test for using other monad than IO as the load time type.
readerTTest :: IO ()
readerTTest = do
    let splices = "foo" ## lift ask >>= return . yieldPureText
        sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                    & scCompiledSplices .~ splices
                    & scTemplateLocations .~ [loadTemplates "templates"]
        hc = HeistConfig sc "" False :: HeistConfig IO (ReaderT T.Text IO)
    ini <- runReaderT (initHeist hc) "hello"
    res <- runExceptT $ do
        hs <- ExceptT . return $ ini
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "readert"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "readert test" (Right "Value for foo is hello\n") res
