module Heist.Compiled.Tests
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
import           Heist
import           Heist.Compiled
import qualified Text.XmlHtml        as X
import qualified Text.XmlHtml.Cursor as X
import           Heist.Compiled.Tutorial

-- NOTE: We can't test compiled templates on the templates directory as it
-- stands today because that directory contains some error conditions such as
-- infinite bind loops, apply tags with no template attribute, and apply tags
-- with ".." in the tag path (which doesn't currently work).

tests :: [Test]
tests = [ testCase     "compiled/simple"  simpleCompiledTest
        , testCase     "compiled/people"  peopleTest
        ]

simpleCompiledTest = do
    res <- runWithStateSplice "templates"
    H.assertBool "compiled state splice" $ res ==
      "<bind tag=\"att\">ultralongname</bind>&#10;<html>&#10;3&#10;</html>&#10;"

peopleTest = do
    res <- personListTest "templates"
    H.assertBool "people splice" $ res ==
      "&#10;<p>Doe, John: 42&#32;years old</p>&#10;&#10;<p>Smith, Jane: 21&#32;years old</p>&#10;&#10;"

