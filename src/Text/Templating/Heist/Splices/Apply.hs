module Text.Templating.Heist.Splices.Apply where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types

------------------------------------------------------------------------------
-- | Default name for the apply splice.
applyTag :: Text
applyTag = "apply"


------------------------------------------------------------------------------
-- | Default attribute name for the apply tag.
applyAttr :: Text
applyAttr = "template"


------------------------------------------------------------------------------
-- | Raw core of apply functionality.  This is abstracted for use in other
-- places like an enhanced (from the original) bind
rawApply :: (Monad m)
         => [X.Node]
         -> TPath
         -> [X.Node]
         -> HeistT m Template
rawApply calledNodes newContext paramNodes = do
    st <- getTS  -- Can't use localTS here because the modifier is not pure
    processedParams <- runNodeList paramNodes
    modifyTS (bindSplice "content" $ return processedParams)
    setContext newContext
    result <- runNodeList calledNodes
    restoreTS st
    return result


------------------------------------------------------------------------------
-- | Applies a template as if the supplied nodes were the children of the
-- <apply> tag.
applyNodes :: Monad m => Template -> Text -> Splice m
applyNodes nodes template = do
    st <- getTS
    maybe (return []) -- TODO: error handling
          (\(t,ctx) -> do
              addDoctype $ maybeToList $ X.docType $ dfDoc t
              rawApply (X.docContent $ dfDoc t) ctx nodes)
          (lookupTemplate (T.encodeUtf8 template) st)


------------------------------------------------------------------------------
-- | Implementation of the apply splice.
applyImpl :: Monad m => Splice m
applyImpl = do
    node <- getParamNode
    case X.getAttribute applyAttr node of
        Nothing   -> return [] -- TODO: error handling
        Just template -> applyNodes (X.childNodes node) template


