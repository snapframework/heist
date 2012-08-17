{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The \"cache\" splice ensures that its contents are cached and only
-- evaluated periodically. The cached contents are returned every time the
-- splice is referenced.
--
-- Use the ttl attribute to set the amount of time between reloads. The ttl
-- value should be a positive integer followed by a single character specifying
-- the units. Valid units are seconds, minutes, hours, days, and weeks. If the
-- ttl string is invalid or the ttl attribute is not specified, the cache is
-- never refreshed (i.e. the splice is evaluated once and never changes
-- thereafter.).

module Heist.Compiled.Splices.Cache
  ( cacheTag
  , cacheImpl
  ) where

import           Blaze.ByteString.Builder
import           Control.Monad.Trans
import           Data.IORef
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Read
import           Data.Time.Clock
import           Text.XmlHtml                  hiding (Node)

import           Heist.Compiled.Internal
import           Heist.Types

------------------------------------------------------------------------------
data CacheRecord = CacheRecord {
      _timestamp :: UTCTime
    , _contents  :: Builder
}


------------------------------------------------------------------------------
cacheTag :: Text
cacheTag = "cache"


------------------------------------------------------------------------------
cacheImpl :: (MonadIO m) => Splice m
cacheImpl = do
    tree            <- getParamNode
    ioRef           <- liftIO $ newIORef Nothing
    childHtmlAction <- promiseChildren

    let ttl = maybe 0 parseTTL $ getAttribute "ttl" tree
    yieldRuntime $ cacheAction ioRef childHtmlAction ttl


------------------------------------------------------------------------------
-- | 
newOutput :: MonadIO m
          => IORef (Maybe CacheRecord) -> m Builder -> UTCTime -> m Builder
newOutput ioRef childHtmlAction now = do
    builder <- childHtmlAction
    liftIO $ writeIORef ioRef $ Just $ CacheRecord now builder
    return builder


------------------------------------------------------------------------------
-- | 
cacheAction :: MonadIO m
            => IORef (Maybe CacheRecord) -> m Builder -> Int -> m Builder
cacheAction ioRef childHtmlAction ttl = do
    now <- liftIO getCurrentTime

    liftIO (readIORef ioRef) >>=
        maybe (newOutput ioRef childHtmlAction now)
              (\(CacheRecord timestamp html) -> do
                   let delta = diffUTCTime now timestamp
                   if ttl > 0 && delta >= fromIntegral ttl
                     then newOutput ioRef childHtmlAction now
                     else return html)


------------------------------------------------------------------------------
-- | Converts a TTL string into an integer number of seconds.
parseTTL :: Text -> Int
parseTTL s = value * multiplier
  where
    value = either (const 0) fst $ decimal s
    multiplier = case T.last s of
        's' -> 1
        'm' -> 60
        'h' -> 3600
        'd' -> 86400
        'w' -> 604800
        _   -> 0
