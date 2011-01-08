{-# LANGUAGE OverloadedStrings #-}

module Text.Templating.Heist.Splices.Apply where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
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
-- | Implementation of the apply splice.
applyImpl :: Monad m => Splice m
applyImpl = do
    node <- getParamNode
    case X.getAttribute applyAttr node of
        Nothing   -> return [] -- TODO: error handling
        Just attr -> do 
            st <- getTS
            maybe (return []) -- TODO: error handling
                  (\(t,ctx) -> do
                      addDoctype $ maybeToList $ X.docType t
                      processedChildren <- runNodeList $ X.childNodes node
                      modifyTS (bindSplice "content" $ return processedChildren)
                      setContext ctx
                      result <- runNodeList $ X.docContent t
                      restoreTS st
                      return result)
                  (lookupTemplate (T.encodeUtf8 attr)
                                  (st {_curContext = nextCtx attr st}))
  where nextCtx name st
            | T.isPrefixOf "/" name = []
            | otherwise             = _curContext st


