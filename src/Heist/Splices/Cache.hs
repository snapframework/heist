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
-- explicitly cleared with clearCacheTagState.
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
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Time.Clock
import           Data.Word
import           System.Random
import           Text.XmlHtml


------------------------------------------------------------------------------
import           Heist.Common
import qualified Heist.Compiled.Internal as C
import           Heist.Interpreted.Internal
import           Heist.Types


------------------------------------------------------------------------------
cacheTagName :: Text
cacheTagName = "cache"


------------------------------------------------------------------------------
-- | State for storing cache tag information
newtype CacheTagState =
    CTS (MVar (HashMap Text (UTCTime, Maybe Template, Maybe Builder)))


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
    (value,rest) = either (const (0::Int,"s")) id $ decimal s
    multiplier = case T.take 1 rest of
        "s" -> 1 :: Int
        "m" -> 60
        "h" -> 3600
        "d" -> 86400
        "w" -> 604800
        _   -> 1


------------------------------------------------------------------------------
-- | This is the splice that actually does the work.  You should bind it to
-- the same tag name as you bound the splice returned by mkCacheTag otherwise
-- it won't work and you'll get runtime errors.
cacheImpl :: (MonadIO n) => CacheTagState -> Splice n
cacheImpl (CTS mv) = do
    tree <- getParamNode
    fp <- getTemplateFilePath
    ctx <- getContext
    let err = error $ unwords ["cacheImpl is bound to a tag"
                              ,"that didn't get an id attribute."
                              ," This should never happen."]
    let i = maybe err id $ getAttribute "id" tree
        ttl = maybe 0 parseTTL $ getAttribute "ttl" tree
    mp <- liftIO $ readMVar mv

    ns <- do
        cur <- liftIO getCurrentTime
        let mbn = H.lookup i mp
            reload builder = do
                nodes' <- runNodeList $ childNodes tree
                let newMap = H.insert i (cur,Just nodes',builder) mp
                liftIO $ modifyMVar_ mv (const $ return newMap)
                return $! nodes'
        case mbn of
            Nothing -> do
                reload Nothing
            (Just (lastUpdate,n,builder)) -> do
                if (ttl > 0 && tagName tree == Just cacheTagName &&
                   diffUTCTime cur lastUpdate > fromIntegral ttl) ||
                   isNothing n
                  then reload builder
                  else do
                      stopRecursion
                      return $! fromJust n

    return ns


------------------------------------------------------------------------------
-- | This is the compiled splice version of cacheImpl.
cacheImplCompiled :: (MonadIO n) => CacheTagState -> C.Splice n
cacheImplCompiled (CTS mv) = do
    tree <- getParamNode
    fp <- getTemplateFilePath
    ctx <- getContext
    let err = error $ unwords ["cacheImplCompiled is bound to a tag"
                              ,"that didn't get an id attribute."
                              ," This should never happen."]
    let i = maybe err id $ getAttribute "id" tree
        ttl = maybe 0 parseTTL $ getAttribute "ttl" tree

    compiled <- C.runNodeList $ childNodes tree
    return $ C.yieldRuntime $ do
        mp <- liftIO $ readMVar mv
        cur <- liftIO getCurrentTime
        let mbn = H.lookup i mp
            reload nodes = do
                out <- C.codeGen compiled
                let newMap = H.insert i (cur,nodes,Just out) mp
                liftIO $ modifyMVar_ mv (const $ return newMap)
                return $! out
        builder <- case mbn of
            Nothing -> do
                reload Nothing
            (Just (lastUpdate,n,builder)) -> do
                if (ttl > 0 && tagName tree == Just cacheTagName &&
                   diffUTCTime cur lastUpdate > fromIntegral ttl) ||
                   isNothing builder
                  then reload n
                  else return $! fromJust builder
        return builder


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
    mv <- liftM CTS $ newMVar H.empty

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

    cf <- getsHS _curTemplateFile
--    liftIO $ print cf
--    when (cf == (Just "snaplets/heist/snap-website/faq.tpl")) $ do
--        liftIO $ putStrLn "======= Before ======="
--        liftIO $ B.putStrLn $ toByteString $ renderHtmlFragment UTF8 $ childNodes node
    newChildren <- runNodeList $ childNodes node
--    when (cf == (Just "snaplets/heist/snap-website/faq.tpl")) $ do
--        liftIO $ putStrLn "======= After ======="
--        liftIO $ B.putStrLn $ toByteString $ renderHtmlFragment UTF8 newChildren
    stopRecursion
    return $ [setAttribute "id" i $ node { elementChildren = newChildren }]


