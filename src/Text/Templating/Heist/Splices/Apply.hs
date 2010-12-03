{-# LANGUAGE OverloadedStrings #-}

module Text.Templating.Heist.Splices.Apply where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import qualified Text.XML.Expat.Tree as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types

------------------------------------------------------------------------------
-- | Default name for the apply splice.
applyTag :: ByteString
applyTag = "apply"


------------------------------------------------------------------------------
-- | Default attribute name for the apply tag.
applyAttr :: ByteString
applyAttr = "template"


------------------------------------------------------------------------------
-- | Implementation of the apply splice.
applyImpl :: Monad m => Splice m
applyImpl = do
    node <- getParamNode
    case X.getAttribute node applyAttr of
        Nothing   -> return [] -- TODO: error handling
        Just attr -> do 
            st <- getTS
            maybe (return []) -- TODO: error handling
                  (\(t,ctx) -> do
                      addDoctype $ maybeToList $ _itDoctype t
                      processedChildren <- runNodeList $ X.getChildren node
                      modifyTS (bindSplice "content" $ return processedChildren)
                      setContext ctx
                      result <- runNodeList $ _itNodes t
                      restoreTS st
                      return result)
                  (lookupTemplate attr (st {_curContext = nextCtx attr st}))
  where nextCtx name st
            | B.isPrefixOf "/" name = []
            | otherwise             = _curContext st


