module Text.Templating.Heist.Splices.Static
{-# DEPRECATED "This will go away in the future.  Use the cache splice instead." #-}
  ( StaticTagState
  , bindStaticTag
  , clearStaticTagCache
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Random
import           Text.XmlHtml.Cursor
import           Text.XmlHtml hiding (Node)


------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types


------------------------------------------------------------------------------
-- | State for storing static tag information
newtype StaticTagState = STS (MVar (Map Text Template))


------------------------------------------------------------------------------
-- | Clears the static tag state.
clearStaticTagCache :: StaticTagState -> IO ()
clearStaticTagCache (STS staticMVar) =
    modifyMVar_ staticMVar (const $ return Map.empty)


------------------------------------------------------------------------------
-- | The \"static\" splice ensures that its contents are evaluated once and
-- then cached.  The cached contents are returned every time the splice is
-- referenced.
staticImpl :: (MonadIO m)
           => StaticTagState
           -> HeistT m Template
staticImpl (STS mv) = do
    tree <- getParamNode
    let i = fromJust $ getAttribute "id" tree

    mp <- liftIO $ readMVar mv

    (mp',ns) <- do
                   let mbn = Map.lookup i mp
                   case mbn of
                       Nothing -> do
                           nodes' <- runNodeList $ childNodes tree
                           return $! (Map.insert i nodes' mp, nodes')
                       (Just n) -> do
                           stopRecursion
                           return $! (mp,n)

    liftIO $ modifyMVar_ mv (const $ return mp')

    return ns


------------------------------------------------------------------------------
-- | Modifies a HeistState to include a \"static\" tag.  The static tag is
-- not bound automatically with the other default Heist tags.  This is because
-- this function also returns StaticTagState, so the user will be able to
-- clear it with the 'clearStaticTagCache' function.
bindStaticTag :: MonadIO m
              => HeistState m
              -> IO (HeistState m, StaticTagState)
bindStaticTag ts = do
    sr <- newIORef $ Set.empty
    mv <- liftM STS $ newMVar Map.empty

    return $ (addOnLoadHook (assignIds sr) $
                bindSplice "static" (staticImpl mv) ts,
              mv)

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
                    return i

          g curs = do
              let node = current curs
              curs' <- if tagName node == Just "static"
                         then do
                             i <- getId
                             return $ modifyNode (setAttribute "id" i) curs
                         else return curs
              let mbc = nextDF curs'
              maybe (return $ topNode curs') g mbc




