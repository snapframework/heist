module Text.Templating.Heist.Splices.BindStrict where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Splices.Apply
import           Text.Templating.Heist.Splices.Bind
import           Text.Templating.Heist.Types

-- | Default name for the bind splice.
bindStrictTag :: Text
bindStrictTag = "bindStrict"


------------------------------------------------------------------------------
-- | Implementation of the bind splice.
bindStrictImpl :: Monad m => Splice m
bindStrictImpl = do
    node <- getParamNode
    cs <- runChildren
    maybe (return ()) (add cs)
          (X.getAttribute bindAttr node)
    return []

  where
    add cs nm = modifyTS $ bindSplice nm $ do
        caller <- getParamNode
        ctx <- getContext
        rawApply cs ctx (X.childNodes caller)
