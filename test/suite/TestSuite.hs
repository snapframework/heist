module Main where

import System.Directory
import Test.Framework (defaultMain, testGroup)

import qualified Heist.Interpreted.Tests
import qualified Heist.Compiled.Tests
import qualified Heist.Tests

main :: IO ()
main = do
    -- Need to change directory after we switched to cabal test infra
    setCurrentDirectory "test"
    defaultMain tests
  where tests = [ testGroup "Heist.Interpreted.Tests"
                            Heist.Interpreted.Tests.tests
                , testGroup "Heist.Compiled.Tests"
                            Heist.Compiled.Tests.tests
                , testGroup "Heist.Tests"
                            Heist.Tests.tests
                ]
