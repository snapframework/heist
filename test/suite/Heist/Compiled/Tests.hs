{-# LANGUAGE OverloadedStrings #-}

module Heist.Compiled.Tests where

import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Lens
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Map.Syntax
import           Data.Monoid
import           Data.Text.Encoding
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H


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
        , testCase     "compiled/nsbind"        nsBindTest
        , testCase     "compiled/nsbinderr"     nsBindErrorTest
        , testCase     "compiled/nsbindstack"   nsBindStackTest
        , testCase     "compiled/doctype"       doctypeTest
        ]

simpleCompiledTest :: IO ()
simpleCompiledTest = do
    res <- runWithStateSplice "templates"
    H.assertEqual "compiled state splice" expected res
  where
    expected =
      mappend doctype "\n&#10;<html>&#10;3&#10;</html>&#10;"

peopleTest :: IO ()
peopleTest = do
    res <- personListTest "templates"
    H.assertEqual "people splice" expected res
  where
    expected =
      "&#10;<p>Doe, John: 42&#32;years old</p>&#10;&#10;<p>Smith, Jane: 21&#32;years old</p>&#10;&#10;"

templateHC :: HeistConfig IO
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
      "<rss><channel><link>http://www.devalot.com/</link></channel></rss>&#10;"

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
    expected = "Alpha\naoeu&#10;Beta\n<h:foo aoeu='htns'>Inside h:foo</h:foo>&#10;End\n"


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
    expected = "Alpha\naoeu&#10;Beta\n<h:foo aoeu='htns'>Inside h:foo</h:foo>&#10;End\n"


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
    expected = "Alpha\n<foo aoeu='htns'>Inside foo</foo>&#10;Beta\naoeu&#10;End\n"


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
    expected = "Alpha\n<foo aoeu='htns'>Inside foo</foo>&#10;Beta\naoeu&#10;End\n"


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


nsBindTemplateHC :: HeistConfig IO
nsBindTemplateHC = HeistConfig sc "h" False
  where
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ nsBindTestSplices
                & scTemplateLocations .~ [loadTemplates "templates-nsbind"]

nsBindTestSplices :: Splices (Splice IO)
nsBindTestSplices = do
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
        hs <- ExceptT $ initHeist $ nsBindTemplateHC
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nsbind"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace bind test" (Right expected)  res
  where
    expected = "Alpha\n&#10;Beta\nasdf&#10;Gamma\n<sub></sub>&#10;&#10;"


------------------------------------------------------------------------------
-- | Test splice error reporting.
nsBindErrorTest :: IO ()
nsBindErrorTest = do
    res <- runExceptT $ do
        hs <- ExceptT $ initHeist $ nsBindTemplateHC
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nsbinderror"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace bind error test" (Left [ err1, err2, err3 ]) res
  where
    err1 = "templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid3\n   ... via templates-nsbind/nsbinderror.tpl: h:main2\nBound splices: h:sub h:recurse h:main2 h:main"
    err2 = "templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid2\n   ... via templates-nsbind/nsbinderror.tpl: h:recurse\n   ... via templates-nsbind/nsbinderror.tpl: h:main\nBound splices: h:sub h:recurse h:main2 h:main"
    err3 = "templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid1\nBound splices: h:main2 h:main"


------------------------------------------------------------------------------
-- | Test splice error data structure.
nsBindStackTest :: IO ()
nsBindStackTest = do
    res <- initHeist nsBindTemplateHC >>=
           return . (either Left (Right . _spliceErrors))

    H.assertEqual "namespace bind stack test" (Right [ err1, err2, err3 ]) res
  where
    err1 = SpliceError [ ( ["nsbinderror"]
                         , Just "templates-nsbind/nsbinderror.tpl"
                         , "h:main2") ]
               (Just "templates-nsbind/nsbinderror.tpl")
               ["h:sub","h:recurse","h:main2","h:main"]
               "No splice bound for h:invalid3"
    err2 = SpliceError [ ( ["nsbinderror"]
                         , Just "templates-nsbind/nsbinderror.tpl"
                         , "h:recurse")
                       , ( ["nsbinderror"]
                         , Just "templates-nsbind/nsbinderror.tpl"
                         ,"h:main") ]
               (Just "templates-nsbind/nsbinderror.tpl")
               ["h:sub","h:recurse","h:main2","h:main"]
               "No splice bound for h:invalid2"
    err3 = SpliceError []
               (Just "templates-nsbind/nsbinderror.tpl")
               ["h:main2","h:main"]
               "No splice bound for h:invalid1"
