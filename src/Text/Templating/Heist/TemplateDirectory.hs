{-|

This module defines a TemplateDirectory data structure for convenient
interaction with templates within web apps.

-}

module Text.Templating.Heist.TemplateDirectory
    ( TemplateDirectory
    , newTemplateDirectory
    , newTemplateDirectory'

    , getDirectoryTS
    , reloadTemplateDirectory
    ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


------------------------------------------------------------------------------
-- | Structure representing a template directory.
data TemplateDirectory m
    = TemplateDirectory
        FilePath
        (HeistState m)
        (MVar (HeistState m))
        StaticTagState


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory' wrapped in an Either for
-- error handling.
newTemplateDirectory :: (MonadIO m, MonadIO n)
                     => FilePath
                     -> HeistState m
                     -> n (Either String (TemplateDirectory m))
newTemplateDirectory dir templateState = liftIO $ do
    (origTs,sts) <- bindStaticTag templateState
    ets <- loadTemplates dir origTs
    leftPass ets $ \ts -> do
        tsMVar <- newMVar $ ts
        return $ TemplateDirectory dir origTs tsMVar sts


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory', using the monad's fail
-- function on error.
newTemplateDirectory' :: (MonadIO m, MonadIO n)
                      => FilePath
                      -> HeistState m
                      -> n (TemplateDirectory m)
newTemplateDirectory' = ((either fail return =<<) .) . newTemplateDirectory


------------------------------------------------------------------------------
-- | Gets the 'HeistState' from a TemplateDirectory.
getDirectoryTS :: (Monad m, MonadIO n)
               => TemplateDirectory m
               -> n (HeistState m)
getDirectoryTS (TemplateDirectory _ _ tsMVar _) = liftIO $ readMVar $ tsMVar


------------------------------------------------------------------------------
-- | Clears cached content and reloads templates from disk.
reloadTemplateDirectory :: (MonadIO m, MonadIO n)
                        => TemplateDirectory m
                        -> n (Either String ())
reloadTemplateDirectory (TemplateDirectory p origTs tsMVar sts) = liftIO $ do
    clearStaticTagCache sts
    ets <- loadTemplates p origTs
    leftPass ets $ \ts -> modifyMVar_ tsMVar (const $ return ts)


------------------------------------------------------------------------------
-- | Prepends an error onto a Left.
leftPass :: Monad m => Either String b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError) (liftM Right . m) e
  where
    loadError = (++) "Error loading templates: "
