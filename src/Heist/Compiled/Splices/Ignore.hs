{-# LANGUAGE OverloadedStrings #-}

module Heist.Compiled.Splices.Ignore
    ( ignoreTag
    , ignoreImpl
    ) where


------------------------------------------------------------------------------
import qualified Data.DList                      as DL
import           Data.Text (Text)

import           Heist.Compiled.Internal


------------------------------------------------------------------------------
-- | Default name for the ignore splice.
ignoreTag :: Text
ignoreTag = "ignore"


------------------------------------------------------------------------------
-- | Implementation of the apply splice.
ignoreImpl :: Monad m => Splice m
ignoreImpl = return DL.empty
