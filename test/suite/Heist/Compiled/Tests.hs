module Heist.Compiled.Tests
  ( tests
  ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H


------------------------------------------------------------------------------
import           Heist.Tutorial.CompiledSplices
import           Heist.TestCommon

-- NOTE: We can't test compiled templates on the templates directory as it
-- stands today because that directory contains some error conditions such as
-- infinite bind loops, apply tags with no template attribute, and apply tags
-- with ".." in the tag path (which doesn't currently work).

tests :: [Test]
tests = [ testCase     "compiled/simple"       simpleCompiledTest
        , testCase     "compiled/people"       peopleTest
        ]

simpleCompiledTest :: IO ()
simpleCompiledTest = do
    res <- runWithStateSplice "templates"
    H.assertEqual "compiled state splice" expected res
  where
    expected =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>\n&#10;<html>&#10;3&#10;</html>&#10;"

peopleTest :: IO ()
peopleTest = do
    res <- personListTest "templates"
    H.assertEqual "people splice" expected res
  where
    expected =
      "&#10;<p>Doe, John: 42&#32;years old</p>&#10;&#10;<p>Smith, Jane: 21&#32;years old</p>&#10;&#10;"

