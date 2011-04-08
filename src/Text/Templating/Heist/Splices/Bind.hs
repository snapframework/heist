module Text.Templating.Heist.Splices.Bind where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Splices.Apply
import           Text.Templating.Heist.Types

-- | Default name for the bind splice.
bindTag :: Text
bindTag = "bind"


------------------------------------------------------------------------------
-- | Default attribute name for the bind tag.
bindAttr :: Text
bindAttr = "tag"


------------------------------------------------------------------------------
-- | Implementation of the bind splice.
bindImpl :: Monad m => Splice m
bindImpl = do
    node <- getParamNode
    maybe (return ())
          (add node)
          (X.getAttribute bindAttr node)
    return []

  where
    add node nm = modifyTS $ bindSplice nm $ do
        caller <- getParamNode
        ctx <- getContext
        rawApply (X.childNodes node) ctx (X.childNodes caller)


