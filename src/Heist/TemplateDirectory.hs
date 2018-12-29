{-# LANGUAGE FlexibleContexts  #-}
{-|

This module defines a TemplateDirectory data structure for convenient
interaction with templates within web apps.

-}

module Heist.TemplateDirectory
    ( TemplateDirectory
    , newTemplateDirectory
    , newTemplateDirectory'

    , getDirectoryHS
    , getDirectoryCTS
    , reloadTemplateDirectory
    ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Heist
import           Heist.Internal.Types
import           Heist.Splices.Cache


------------------------------------------------------------------------------
-- | Structure representing a template directory.
data TemplateDirectory n m
    = TemplateDirectory
        FilePath
        (HeistConfig n m)
        (MVar (HeistState n m))
        (MVar CacheTagState)


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory' wrapped in an Either for
-- error handling.
newTemplateDirectory
    :: (MonadIO n, MonadIO m, MonadBaseControl IO m)
    => FilePath
    -> HeistConfig n m
    -- namespaced tag.
    -> m (Either [String] (TemplateDirectory n m))
newTemplateDirectory dir hc = do
    let sc = (_hcSpliceConfig hc) { _scTemplateLocations = [loadTemplates dir] }
    let hc' = hc { _hcSpliceConfig = sc }
    epair <- initHeistWithCacheTag hc'
    case epair of
      Left es -> return $ Left es
      Right (hs,cts) -> do
        tsMVar <- liftIO $ newMVar hs
        ctsMVar <- liftIO $ newMVar cts
        return $ Right $ TemplateDirectory dir hc' tsMVar ctsMVar


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory', using the monad's fail
-- function on error.
newTemplateDirectory'
    :: (MonadIO n, MonadIO m, MonadBaseControl IO m)
    => FilePath
    -> HeistConfig n m
    -> m (TemplateDirectory n m)
newTemplateDirectory' dir hc = do
    res <- newTemplateDirectory dir hc
    either (error . concat) return res


------------------------------------------------------------------------------
-- | Gets the 'HeistState' from a TemplateDirectory.
getDirectoryHS :: (MonadIO n, MonadIO m)
               => TemplateDirectory n m
               -> IO (HeistState n m)
getDirectoryHS (TemplateDirectory _ _ tsMVar _) =
    liftIO $ readMVar $ tsMVar


------------------------------------------------------------------------------
-- | Clears the TemplateDirectory's cache tag state.
getDirectoryCTS :: TemplateDirectory n m -> IO CacheTagState
getDirectoryCTS (TemplateDirectory _ _ _ ctsMVar) = readMVar ctsMVar


------------------------------------------------------------------------------
-- | Clears cached content and reloads templates from disk.
reloadTemplateDirectory :: (MonadIO n, MonadIO m, MonadBaseControl IO m)
                        => TemplateDirectory n m
                        -> m (Either String ())
reloadTemplateDirectory (TemplateDirectory p hc tsMVar ctsMVar) = do
    let sc = (_hcSpliceConfig hc) { _scTemplateLocations = [loadTemplates p] }
    ehs <- initHeistWithCacheTag (hc { _hcSpliceConfig = sc })
    liftIO $ leftPass ehs $ \(hs,cts) -> do
        modifyMVar_ tsMVar (const $ return hs)
        modifyMVar_ ctsMVar (const $ return cts)


------------------------------------------------------------------------------
-- | Prepends an error onto a Left.
leftPass :: Monad m => Either [String] b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError . concat)
                      (liftM Right . m) e
  where
    loadError = (++) ("Error loading templates: " :: String)
