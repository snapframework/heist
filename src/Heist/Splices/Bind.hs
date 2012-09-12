module Heist.Splices.Bind where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Interpreted.Internal
import           Heist.Splices.Apply
import           Heist.Types

-- | Default name for the bind splice.
bindTag :: Text
bindTag = "bind"


------------------------------------------------------------------------------
-- | Default attribute name for the bind tag.
bindAttr :: Text
bindAttr = "tag"


------------------------------------------------------------------------------
-- | Implementation of the bind splice.
bindImpl :: MonadIO n => Splice n
bindImpl = do
    node <- getParamNode
    let err = "must supply \"" ++ T.unpack bindAttr ++
              "\" attribute in <" ++ T.unpack (X.elementTag node) ++ ">"
    maybe (return () `orError` err)
          (add node)
          (X.getAttribute bindAttr node)
    return []
  where
    add node nm = modifyHS $ bindSplice nm $ do
        caller <- getParamNode
        ctx <- getContext
        rawApply "bind-content" (X.childNodes node)
                 Nothing ctx (X.childNodes caller)

