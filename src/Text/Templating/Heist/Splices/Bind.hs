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
bindImpl = generalBind add
  where
    add node nm = do
        modifyTS $ bindSplice nm (return $ X.childNodes node)
        return []


generalBind :: Monad m => (X.Node -> Text -> Splice m) -> Splice m
generalBind k = do
    node <- getParamNode
    maybe (return [])
          (k node)
          (X.getAttribute bindAttr node)


bindImplNew :: Monad m => Splice m
bindImplNew = generalBind action
  where
    action node nm = do --unused nm might be a bug
        caller <- getParamNode
        ctx <- getContext
        rawApply (X.childNodes node) ctx (X.childNodes caller)

