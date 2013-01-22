{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The \"cache\" splice ensures that its contents are cached and only
-- evaluated periodically.  The cached contents are returned every time the
-- splice is referenced.
--
-- Use the ttl attribute to set the amount of time between reloads.  The ttl
-- value should be a positive integer followed by a single character
-- specifying the units.  Valid units are a single letter abbreviation for one
-- of seconds, minutes, hours, days, and weeks.  If the ttl string is invalid
-- or the ttl attribute is not specified, the cache is never refreshed unless
-- explicitly cleared with clearCacheTagState.  The compiled splice version of
-- the cache tag does not require a cache tag state, so clearCacheTagState
-- will not work for compiled cache tags.

module Heist.Splices.Cache
  ( CacheTagState
  , cacheImpl
  , cacheImplCompiled
  , mkCacheTag
  , clearCacheTagState
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Time.Clock
import           Data.Word
import           System.Random
import           Text.XmlHtml


------------------------------------------------------------------------------
import qualified Heist.Compiled.Internal as C
import           Heist.Interpreted.Internal
import           Heist.Types


------------------------------------------------------------------------------
cacheTagName :: Text
cacheTagName = "cache"


------------------------------------------------------------------------------
-- | State for storing cache tag information
newtype CacheTagState =
    CTS (MVar ([IORef (Maybe (UTCTime, Builder))], HashMap Text (UTCTime, Template)))


addCompiledRef :: IORef (Maybe (UTCTime, Builder)) -> CacheTagState -> IO ()
addCompiledRef ref (CTS mv) = do
    modifyMVar_ mv (\(a,b) -> return (ref:a, b))

    
------------------------------------------------------------------------------
-- | Clears the cache tag state.
clearCacheTagState :: CacheTagState -> IO ()
clearCacheTagState (CTS cacheMVar) = do
    refs <- modifyMVar cacheMVar (\(a,_) -> return ((a, H.empty), a))
    mapM_ (\ref -> writeIORef ref Nothing) refs


------------------------------------------------------------------------------
-- | Converts a TTL string into an integer number of seconds.
parseTTL :: Text -> Int
parseTTL s = value * multiplier
  where
    (value,rest) = either (const (0::Int,"s")) id $ decimal s
    multiplier = case T.take 1 rest of
        "s" -> 1 :: Int
        "m" -> 60
        "h" -> 3600
        "d" -> 86400
        "w" -> 604800
        _   -> 1


getTTL :: Node -> NominalDiffTime
getTTL tree = fromIntegral $ maybe 0 parseTTL $ getAttribute "ttl" tree
{-# INLINE getTTL #-}


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
        !ttl = getTTL tree
    mp <- liftIO $ readMVar mv

    ns <- do
        cur <- liftIO getCurrentTime
        let mbn = H.lookup i $ snd mp
            reload = do
                nodes' <- runNodeList $ childNodes tree
                let newMap = H.insert i (cur, nodes') $ snd mp
                liftIO $ modifyMVar_ mv (\(a,_) -> return (a, newMap))
                return $! nodes'
        case mbn of
            Nothing -> reload
            (Just (lastUpdate,n)) -> do
                if ttl > 0 && tagName tree == Just cacheTagName &&
                   diffUTCTime cur lastUpdate > ttl
                  then reload
                  else do
                      stopRecursion
                      return $! n

    return ns


------------------------------------------------------------------------------
-- | This is the compiled splice version of cacheImpl.
cacheImplCompiled :: (MonadIO n) => CacheTagState -> C.Splice n
cacheImplCompiled cts = do
    tree <- getParamNode
    let !ttl = getTTL tree

    compiled <- C.runNodeList $ childNodes tree
    ref <- liftIO $ newIORef Nothing
    liftIO $ addCompiledRef ref cts
    let reload curTime = do
            builder <- C.codeGen compiled
            let out = fromByteString $! toByteString $! builder
            liftIO $ writeIORef ref (Just (curTime, out))
            return $! out
    return $ C.yieldRuntime $ do
        mbn <- liftIO $ readIORef ref
        cur <- liftIO getCurrentTime
        case mbn of
            Nothing -> reload cur
            (Just (lastUpdate,bs)) -> do
                if (ttl > 0 && diffUTCTime cur lastUpdate > ttl)
                  then reload cur
                  else return $! bs


------------------------------------------------------------------------------
-- | Returns items necessary to set up a \"cache\" tag.  The cache tag cannot
-- be bound automatically with the other default Heist tags.  This is because
-- this function also returns CacheTagState, so the user will be able to clear
-- it with the 'clearCacheTagState' function.
--
-- This function returns a splice and a CacheTagState.  The splice is of type
-- @Splice IO@ because it has to be bound as a load time preprocessing splice.
-- Haskell's type system won't allow you to screw up and pass this splice as
-- the wrong argument to initHeist.
mkCacheTag :: IO (Splice IO, CacheTagState)
mkCacheTag = do
    sr <- newIORef $ Set.empty
    mv <- liftM CTS $ newMVar ([], H.empty)

    return $ (setupSplice sr, mv)


------------------------------------------------------------------------------
-- | Explicit type signature to avoid the Show polymorphism problem.
generateId :: IO Word
generateId = getStdRandom random


------------------------------------------------------------------------------
-- | Gets a unique ID for use in the cache tags.
getId :: IORef (Set.HashSet Text) -> IO Text
getId setref = do
    i <- liftM (T.pack . show) generateId
    _set <- readIORef setref
    if Set.member i _set
      then getId setref
      else do
          writeIORef setref $ Set.insert i _set
          return $ T.append "cache-id-" i


------------------------------------------------------------------------------
-- | A splice that sets the id attribute so that nodes can be cache-aware.
setupSplice :: IORef (Set.HashSet Text) -> Splice IO
setupSplice setref = do
    i <- liftIO $ getId setref
    node <- getParamNode

    newChildren <- runNodeList $ childNodes node
    stopRecursion
    return $ [setAttribute "id" i $ node { elementChildren = newChildren }]


