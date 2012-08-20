-- | The \"cache\" splice ensures that its contents are cached and only
-- evaluated periodically.  The cached contents are returned every time the
-- splice is referenced.
--
-- Use the ttl attribute to set the amount of time between reloads.  The ttl
-- value should be a positive integer followed by a single character
-- specifying the units.  Valid units are a single letter abbreviation for one
-- of seconds, minutes, hours, days, and weeks.  If the ttl string is invalid
-- or the ttl attribute is not specified, the cache is never refreshed unless
-- explicitly cleared with clearCacheTagState.
module Heist.Interpreted.Splices.Cache
  ( CacheTagState
  , cacheImpl
  , cacheImplCompiled 
  , mkCacheTag
  , clearCacheTagState
  , initHeistWithCacheTag 
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Time.Clock
import           System.Random
import           Text.XmlHtml hiding (Node)


------------------------------------------------------------------------------
import qualified Heist.Compiled.Internal as C
import           Heist.Interpreted.Internal
import           Heist.Types
import           Heist


------------------------------------------------------------------------------
cacheTagName :: Text
cacheTagName = "cache"


------------------------------------------------------------------------------
-- | State for storing cache tag information
newtype CacheTagState =
    CTS (MVar (HashMap Text (UTCTime, Template, Builder)))


------------------------------------------------------------------------------
-- | Clears the cache tag state.
clearCacheTagState :: CacheTagState -> IO ()
clearCacheTagState (CTS cacheMVar) =
    modifyMVar_ cacheMVar (const $ return H.empty)


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
-- | This is the splice that actually does the work.  You should bind it to
-- the same tag name as you bound the splice returned by mkCacheTag otherwise
-- it won't work and you'll get runtime errors.
cacheImpl :: (MonadIO n) => CacheTagState -> Splice n
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
        let mbn = H.lookup i mp
            reload builder = do
                nodes' <- runNodeList $ childNodes tree
                return $! ( H.insert i (cur,nodes',builder) mp
                          , nodes')
        case mbn of
            Nothing -> reload mempty
            (Just (lastUpdate,n,builder)) -> do
                if ttl > 0 && tagName tree == Just cacheTagName &&
                   diffUTCTime cur lastUpdate > fromIntegral ttl
                  then reload builder
                  else do
                      stopRecursion
                      return $! (mp,n)

    liftIO $ modifyMVar_ mv (const $ return mp')

    return ns


------------------------------------------------------------------------------
-- | This is the compiled splice version of cacheImpl.
cacheImplCompiled :: (MonadIO n) => CacheTagState -> C.Splice n
cacheImplCompiled (CTS mv) = do
    tree <- getParamNode
    let err = error $ unwords ["cacheImplCompiled is bound to a tag"
                              ,"that didn't get an id attribute."
                              ," This should never happen."]
    let i = maybe err id $ getAttribute "id" tree
        ttl = maybe 0 parseTTL $ getAttribute "ttl" tree

    compiled <- C.runNodeList $ childNodes tree
    C.yieldRuntime $ do
        mp <- liftIO $ readMVar mv
        cur <- liftIO getCurrentTime
        let mbn = H.lookup i mp
            reload nodes = do
                out <- C.codeGen compiled
                return $! (H.insert i (cur,nodes,out) mp, out)
        (mp',builder) <- case mbn of
            Nothing -> reload []
            (Just (lastUpdate,n,builder)) -> do
                if ttl > 0 && tagName tree == Just cacheTagName &&
                   diffUTCTime cur lastUpdate > fromIntegral ttl
                  then reload n
                  else return $! (mp, builder)
        liftIO $ modifyMVar_ mv (const $ return mp')
        return builder


------------------------------------------------------------------------------
-- | Returns items necessary to set up a \"cache\" tag.  The cache tag cannot
-- be bound automatically with the other default Heist tags.  This is because
-- this function also returns CacheTagState, so the user will be able to clear
-- it with the 'clearCacheTagState' function.
--
-- This function returns a splice and a CacheTagState.  The splice is of type
-- @Splice IO@ because it has to be bound as load time preprocessing splice.
-- Haskell's type system won't allow you to screw up and pass this splice as
-- the wrong argument to initHeist.
mkCacheTag :: IO (Splice IO, CacheTagState)
mkCacheTag = do
    sr <- newIORef $ Set.empty
    mv <- liftM CTS $ newMVar H.empty

    return $ (setupSplice sr, mv)


------------------------------------------------------------------------------
-- | Explicit type signature to avoid the Show polymorphism problem.
generateId :: IO Int
generateId = getStdRandom random


------------------------------------------------------------------------------
-- | Gets a unique ID for use in the cache tags.
getId :: IORef (Set.HashSet Text) -> IO Text
getId setref = do
    i <- liftM (T.pack . show) generateId
    st <- readIORef setref
    if Set.member i st
      then getId setref
      else do
          writeIORef setref $ Set.insert i st
          return $ T.append "cache-id-" i


------------------------------------------------------------------------------
-- | A splice that sets the id attribute so that nodes can be cache-aware.
setupSplice :: IORef (Set.HashSet Text) -> Splice IO
setupSplice setref = do
    i <- liftIO $ getId setref
    node <- getParamNode
    return $ [setAttribute "id" i node]


------------------------------------------------------------------------------
-- | This function is the easiest way to set up your HeistState with a cache
-- tag.  It sets up all the necessary splices properly.  If you need to do
-- configure the cache tag differently than how this function does it, you
-- will still probably want to pattern your approach after this function's
-- implementation.
initHeistWithCacheTag :: MonadIO n
                      => [(Text, Splice n)]
                      -- ^ Runtime splices
                      -> [(Text, Splice IO)]
                      -- ^ Static loadtime splices
                      -> [(Text, C.Splice n)]
                      -- ^ Dynamic loadtime splices
                      -> HashMap TPath DocumentFile
                      -> EitherT [String] IO (HeistState n, CacheTagState)
initHeistWithCacheTag rSplices sSplices dSplices rawTemplates = do
    (ss, cts) <- liftIO mkCacheTag
    let tag = "cache"
    hs <- initHeist ((tag, cacheImpl cts) : rSplices)
                    ((tag, ss) : sSplices)
                    ((tag, cacheImplCompiled cts) : dSplices)
                    rawTemplates
    return (hs, cts)

