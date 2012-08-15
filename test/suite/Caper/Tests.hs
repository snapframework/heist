module Caper.Tests
  ( tests
  ) where

import           Blaze.ByteString.Builder
import           Control.Monad.State
import qualified Control.Monad.Trans.State as ST
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           System.IO.Unsafe
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


------------------------------------------------------------------------------
import           Text.Templating.Heist
import           Text.Templating.Heist.Common
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types
import           Text.Templating.Heist.Splices.Apply
import           Text.Templating.Heist.Splices.Ignore
import           Text.Templating.Heist.Splices.Json
import           Text.Templating.Heist.Splices.Markdown
import qualified Text.XmlHtml        as X
import qualified Text.XmlHtml.Cursor as X
import qualified Caper as C
import           Caper.Tutorial

tests :: [Test]
tests = [ testCase     "caper/simple"  simpleCaperTest
        , testCase     "caper/people"  peopleTest
        ]

simpleCaperTest = do
    res <- runWithStateSplice "templates"
    H.assertBool "caper state splice" $ res ==
      "<bind tag=\"att\">ultralongname</bind>&#10;<html>&#10;3&#10;</html>&#10;"

peopleTest = do
    res <- personListTest "templates"
    H.assertBool "people splice" $ res ==
      "&#10;<p>Doe, John: 42&#32;years old</p>&#10;&#10;<p>Smith, Jane: 21&#32;years old</p>&#10;&#10;"

