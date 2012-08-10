module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Templating.Heist.Tests
import qualified Caper.Tests

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "Text.Templating.Heist.Tests"
                            Text.Templating.Heist.Tests.tests
                , testGroup "Caper.Tests"
                            Caper.Tests.tests
                ]
