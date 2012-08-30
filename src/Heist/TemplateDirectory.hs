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
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans
import           Data.Text (Text)
import           Heist
import qualified Heist.Compiled.Internal as C
import qualified Heist.Interpreted.Internal as I
import           Heist.Splices.Cache


------------------------------------------------------------------------------
-- | Structure representing a template directory.
data TemplateDirectory n
    = TemplateDirectory
        FilePath
        [(Text, I.Splice n)]
        [(Text, I.Splice IO)]
        [(Text, C.Splice n)]
        [(Text, AttrSplice n)]
        (MVar (HeistState n))
        (MVar CacheTagState)


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory' wrapped in an Either for
-- error handling.
newTemplateDirectory :: MonadIO n
                     => FilePath
                     -> [(Text, I.Splice n)]
                     -> [(Text, I.Splice IO)]
                     -> [(Text, C.Splice n)]
                     -> [(Text, AttrSplice n)]
                     -> EitherT [String] IO (TemplateDirectory n)
newTemplateDirectory dir a b c d = do
    (hs,cts) <- loadTemplates dir >>= initHeistWithCacheTag a b c d
    tsMVar <- liftIO $ newMVar hs
    ctsMVar <- liftIO $ newMVar cts
    return $ TemplateDirectory dir a b c d tsMVar ctsMVar


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory', using the monad's fail
-- function on error.
newTemplateDirectory' :: MonadIO n
                      => FilePath
                      -> [(Text, I.Splice n)]
                      -> [(Text, I.Splice IO)]
                      -> [(Text, C.Splice n)]
                      -> [(Text, AttrSplice n)]
                      -> IO (TemplateDirectory n)
newTemplateDirectory' dir rSplices sSplices dSplices aSplices = do
    res <- runEitherT $ 
        newTemplateDirectory dir rSplices sSplices dSplices aSplices
    either (error . concat) return res


------------------------------------------------------------------------------
-- | Gets the 'HeistState' from a TemplateDirectory.
getDirectoryHS :: (MonadIO n)
               => TemplateDirectory n
               -> IO (HeistState n)
getDirectoryHS (TemplateDirectory _ _ _ _ _ tsMVar _) =
    liftIO $ readMVar $ tsMVar


------------------------------------------------------------------------------
-- | Clears the TemplateDirectory's cache tag state.
getDirectoryCTS :: TemplateDirectory n -> IO CacheTagState
getDirectoryCTS (TemplateDirectory _ _ _ _ _ _ ctsMVar) = readMVar ctsMVar


------------------------------------------------------------------------------
-- | Clears cached content and reloads templates from disk.
reloadTemplateDirectory :: (MonadIO n)
                        => TemplateDirectory n
                        -> IO (Either String ())
reloadTemplateDirectory (TemplateDirectory p a b c d tsMVar ctsMVar) = do
    ehs <- runEitherT $ loadTemplates p >>= initHeistWithCacheTag a b c d
    leftPass ehs $ \(hs,cts) -> do
        modifyMVar_ tsMVar (const $ return hs)
        modifyMVar_ ctsMVar (const $ return cts)


------------------------------------------------------------------------------
-- | Prepends an error onto a Left.
leftPass :: Monad m => Either [String] b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError . concat)
                      (liftM Right . m) e
  where
    loadError = (++) "Error loading templates: "
