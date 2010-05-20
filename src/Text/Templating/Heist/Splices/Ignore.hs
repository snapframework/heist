{-# LANGUAGE OverloadedStrings #-}

module Text.Templating.Heist.Splices.Ignore where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal


------------------------------------------------------------------------------
-- | Default name for the ignore splice.
ignoreTag :: ByteString
ignoreTag = "ignore"


------------------------------------------------------------------------------
-- | The ignore tag and everything it surrounds disappears in the
-- rendered output.
ignoreImpl :: Monad m => Splice m
ignoreImpl = return []


