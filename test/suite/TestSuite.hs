module Main where


------------------------------------------------------------------------------
import           Control.Monad                  (liftM)
import           Data.List                      (isPrefixOf)
import           System.Directory               (findExecutable,
                                                 setCurrentDirectory)
import           System.Process                 (readProcess)
import           Test.Framework                 (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Heist.Compiled.Tests
import qualified Heist.Interpreted.Pandoc.Tests
import qualified Heist.Interpreted.Tests
import qualified Heist.Tests
------------------------------------------------------------------------------


------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Need to change directory after we switched to cabal test infra
    setCurrentDirectory "test"
    skipPandocTests <- pandoc1Unavailable
    defaultMain (tests ++ if skipPandocTests then [] else [pandocTest])
  where
    tests = [ testGroup "Heist.Interpreted.Tests"
                        Heist.Interpreted.Tests.tests
            , testGroup "Heist.Compiled.Tests"
                        Heist.Compiled.Tests.tests
            , testGroup "Heist.Tests"
                        Heist.Tests.tests
            ]
    pandocTest = testGroup "Heist.Interpreted.Pandoc.Tests"
                           Heist.Interpreted.Pandoc.Tests.tests


------------------------------------------------------------------------------
pandoc1Unavailable :: IO Bool
pandoc1Unavailable =
    maybe (return True) (liftM not1 . version) =<< findExecutable "pandoc"
  where
    version path = readProcess path ["--version"] ""
    not1 = not . ("pandoc 1" `isPrefixOf`)

