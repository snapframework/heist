module Text.Templating.Heist.Splices.Cache
  ( CacheTagState
  , mkCacheTag
  , clearCacheTagState
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Time.Clock
import           System.Random
import           Text.XmlHtml.Cursor
import           Text.XmlHtml hiding (Node)


------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types


------------------------------------------------------------------------------
cacheTagName :: Text
cacheTagName = "cache"


------------------------------------------------------------------------------
-- | State for storing cache tag information
newtype CacheTagState = CTS (MVar (Map Text (UTCTime, Template)))


------------------------------------------------------------------------------
-- | Clears the cache tag state.
clearCacheTagState :: CacheTagState -> IO ()
clearCacheTagState (CTS cacheMVar) =
    modifyMVar_ cacheMVar (const $ return Map.empty)


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

------------------------------------------------------------------------------
-- | The \"cache\" splice ensures that its contents are cached and only
-- evaluated periodically.  The cached contents are returned every time the
-- splice is referenced.
--
-- Use the ttl attribute to set the amount of time between reloads.  The ttl
-- value should be a positive integer followed by a single character
-- specifying the units.  Valid units are seconds, minutes, hours, days, and
-- weeks.  If the ttl string is invalid or the ttl attribute is not specified,
-- the cache is never refreshed unless explicitly cleared with
-- clearCacheTagState.
cacheImpl :: (MonadIO m)
           => CacheTagState
           -> HeistT m Template
cacheImpl (CTS mv) = do
    tree <- getParamNode
    let err = error $ unwords ["cacheImpl is bound to a tag"
                              ,"that didn't get an id attribute."
                              ," This should never happen."]
    let i = maybe err id $ getAttribute "id" tree
        ttl = maybe 0 parseTTL $ getAttribute "ttl" tree
    mp <- liftIO $ readMVar mv

    (mp',ns) <- do
                   cur <- liftIO getCurrentTime
                   let mbn = Map.lookup i mp
                       reload = do
                           nodes' <- runNodeList $ childNodes tree
                           return $! (Map.insert i (cur,nodes') mp, nodes')
                   case mbn of
                       Nothing -> reload
                       (Just (lastUpdate,n)) -> do
                           if ttl > 0 && tagName tree == Just cacheTagName &&
                              diffUTCTime cur lastUpdate > fromIntegral ttl
                             then reload
                             else do
                                 stopRecursion
                                 return $! (mp,n)

    liftIO $ modifyMVar_ mv (const $ return mp')

    return ns


------------------------------------------------------------------------------
-- | Returns a function that modifies a HeistState to include a \"cache\"
-- tag.  The cache tag is not bound automatically with the other default Heist
-- tags.  This is because this function also returns CacheTagState, so the
-- user will be able to clear it with the 'clearCacheTagState' function.
mkCacheTag :: MonadIO m
           => IO (HeistState m -> HeistState m, CacheTagState)
mkCacheTag = do
    sr <- newIORef $ Set.empty
    mv <- liftM CTS $ newMVar Map.empty

    return $ ( addOnLoadHook (assignIds sr) .
               -- The cache tag allows the ttl attribute.
               bindSplice cacheTagName (cacheImpl mv) .
               -- Like the old static tag...does not allow ttl
               bindSplice "static" (cacheImpl mv)
             , mv)

  where
    generateId :: IO Int
    generateId = getStdRandom random

    assignIds setref = mapM f
        where
          f node = g $ fromNode node

          getId = do
              i  <- liftM (T.pack . show) generateId
              st <- readIORef setref
              if Set.member i st
                then getId
                else do
                    writeIORef setref $ Set.insert i st
                    return $ T.append "cache-id-" i

          g curs = do
              let node = current curs
              curs' <- if tagName node == Just cacheTagName ||
                          tagName node == Just "static"
                         then do
                             i <- getId
                             return $ modifyNode (setAttribute "id" i) curs
                         else return curs
              let mbc = nextDF curs'
              maybe (return $ topNode curs') g mbc




