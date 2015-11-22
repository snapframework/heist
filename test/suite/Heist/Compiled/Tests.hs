{-# LANGUAGE OverloadedStrings #-}

module Heist.Compiled.Tests
  ( tests
  ) where

import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Lens
import           Control.Monad.Trans
import           Data.Map.Syntax
import           Data.Monoid
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H


------------------------------------------------------------------------------
import           Heist
import           Heist.Compiled
import           Heist.Internal.Types
import           Heist.Tutorial.CompiledSplices
import           Heist.TestCommon

-- NOTE: We can't test compiled templates on the templates directory as it
-- stands today because that directory contains some error conditions such as
-- infinite bind loops, apply tags with no template attribute, and apply tags
-- with ".." in the tag path (which doesn't currently work).

tests :: [Test]
tests = [ testCase     "compiled/simple"       simpleCompiledTest
        , testCase     "compiled/people"       peopleTest
        , testCase     "compiled/namespace1"    namespaceTest1
        , testCase     "compiled/namespace2"    namespaceTest2
        , testCase     "compiled/namespace3"    namespaceTest3
        , testCase     "compiled/namespace4"    namespaceTest4
        , testCase     "compiled/namespace5"    namespaceTest5
        , testCase     "compiled/nsbind"        nsBindTest
        , testCase     "compiled/nsbinderr"     nsBindErrorTest
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
      mappend doctype "\n&#10;<p>Doe, John: 42&#32;years old</p>&#10;&#10;<p>Smith, Jane: 21&#32;years old</p>&#10;&#10;"

templateHC :: HeistConfig IO
templateHC = HeistConfig sc "" False
  where
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ ("foo" ## return (yieldPureText "aoeu"))
                & scTemplateLocations .~ [loadTemplates "templates"]

namespaceTest1 :: IO ()
namespaceTest1 = do
    res <- runEitherT $ do
        hs <- initHeist templateHC
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test" (Right expected) res
  where
    expected = mappend doctype "\nAlpha\naoeu&#10;Beta\n<h:foo aoeu='htns'>Inside h:foo</h:foo>&#10;End\n"


namespaceTest2 :: IO ()
namespaceTest2 = do
    res <- runEitherT $ do
        hs <- initHeist $ templateHC & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test" (Right expected) res
  where
    expected = mappend doctype "\nAlpha\naoeu&#10;Beta\n<h:foo aoeu='htns'>Inside h:foo</h:foo>&#10;End\n"


namespaceTest3 :: IO ()
namespaceTest3 = do
    res <- runEitherT $ do
        hs <- initHeist $ templateHC & hcNamespace .~ "h"
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test" (Right expected) res
  where
    expected = mappend doctype "\nAlpha\n<foo aoeu='htns'>Inside foo</foo>&#10;Beta\naoeu&#10;End\n"


namespaceTest4 :: IO ()
namespaceTest4 = do
    res <- runEitherT $ do
        hs <- initHeist $ templateHC & hcNamespace .~ "h"
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test" (Right expected) res
  where
    expected = mappend doctype "\nAlpha\n<foo aoeu='htns'>Inside foo</foo>&#10;Beta\naoeu&#10;End\n"


namespaceTest5 :: IO ()
namespaceTest5 = do
    res <- runEitherT $ do
        hs <- initHeist $ templateHC & hcNamespace .~ "h"
                                     & hcCompiledSplices .~ mempty
                                     & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "namespaces"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace test" (Left ["templates/namespaces.tpl: No splice bound for h:foo"]) res


nsBindTemplateHC :: HeistConfig IO
nsBindTemplateHC = HeistConfig sc "h" False
  where
    sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                & scCompiledSplices .~ nsBindTestSplices
                & scTemplateLocations .~ [loadTemplates "templates-nsbind"]

nsBindTestSplices :: Splices (Splice IO)
nsBindTestSplices = "main" ## do
    tpl <- withSplices runChildren nsBindSubSplices (return ())
    return $ yieldRuntime $ codeGen tpl

nsBindSubSplices :: Splices (RuntimeSplice IO () -> Splice IO)
nsBindSubSplices = mapV (pureSplice . textSplice) $
    "sub" ## const "asdf"


nsBindTest :: IO ()
nsBindTest = do
    res <- runEitherT $ do
        hs <- initHeist $ nsBindTemplateHC
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nsbind"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace bind test" (Right expected)  res
  where
    expected = "Alpha\n&#10;Beta\nasdf&#10;Gamma\n<sub></sub>&#10;&#10;"


nsBindErrorTest :: IO ()
nsBindErrorTest = do
    res <- runEitherT $ do
        hs <- initHeist $ nsBindTemplateHC
                           & hcErrorNotBound .~ True
        runner <- noteT ["Error rendering"] $ hoistMaybe $
                    renderTemplate hs "nsbinderror"
        b <- lift $ fst runner
        return $ toByteString b

    H.assertEqual "namespace bind error test" (Left ["templates-nsbind/nsbinderror.tpl: No splice bound for h:invalid"])  res
