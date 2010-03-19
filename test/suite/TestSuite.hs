module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Templating.Heist.Tests
main :: IO ()
main = defaultMain tests
  where tests = [
                  testGroup "Text.Templating.Heist.Tests"
                            Text.Templating.Heist.Tests.tests
                ]
