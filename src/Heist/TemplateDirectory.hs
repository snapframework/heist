{-|

This module defines a TemplateDirectory data structure for convenient
interaction with templates within web apps.

-}

module Heist.TemplateDirectory
    ( TemplateDirectory
    , newTemplateDirectory
    , newTemplateDirectory'

    , getDirectoryTS
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
import           Heist.Interpreted.Splices.Cache


------------------------------------------------------------------------------
-- | Structure representing a template directory.
data TemplateDirectory n
    = TemplateDirectory
        FilePath
        [(Text, I.Splice n)]
        [(Text, I.Splice IO)]
        [(Text, C.Splice n)]
        (MVar (HeistState n))
        CacheTagState


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory' wrapped in an Either for
-- error handling.
newTemplateDirectory :: MonadIO n
                     => FilePath
                     -> [(Text, I.Splice n)]
                     -> [(Text, I.Splice IO)]
                     -> [(Text, C.Splice n)]
                     -> EitherT [String] IO (TemplateDirectory n)
newTemplateDirectory dir a b c = do
    (ss, cts) <- liftIO mkCacheTag
    let a' = ("cache", cacheImpl cts) : a
        b' = ("cache", ss) : b
    hs <- loadTemplates dir >>= initHeist a' b' c
    tsMVar <- liftIO $ newMVar hs
    return $ TemplateDirectory dir a' b' c tsMVar cts


------------------------------------------------------------------------------
-- | Creates and returns a new 'TemplateDirectory', using the monad's fail
-- function on error.
newTemplateDirectory' :: MonadIO n
                      => FilePath
                      -> [(Text, I.Splice n)]
                      -> [(Text, I.Splice IO)]
                      -> [(Text, C.Splice n)]
                      -> IO (TemplateDirectory n)
newTemplateDirectory' dir rSplices sSplices dSplices = do
    res <- runEitherT $ 
        newTemplateDirectory dir rSplices sSplices dSplices
    either (error . concat) return res


------------------------------------------------------------------------------
-- | Gets the 'HeistState' from a TemplateDirectory.
getDirectoryTS :: (MonadIO n)
               => TemplateDirectory n
               -> n (HeistState n)
getDirectoryTS (TemplateDirectory _ _ _ _ tsMVar _) =
    liftIO $ readMVar $ tsMVar


------------------------------------------------------------------------------
-- | Clears cached content and reloads templates from disk.
reloadTemplateDirectory :: (MonadIO n)
                        => TemplateDirectory n
                        -> n (Either String ())
reloadTemplateDirectory (TemplateDirectory p a b c tsMVar _) = liftIO $ do
    ehs <- runEitherT $ loadTemplates p >>= initHeist a b c
    leftPass ehs $ \hs -> modifyMVar_ tsMVar (const $ return hs)


------------------------------------------------------------------------------
-- | Prepends an error onto a Left.
leftPass :: Monad m => Either [String] b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError . concat)
                      (liftM Right . m) e
  where
    loadError = (++) "Error loading templates: "
