{-# LANGUAGE OverloadedStrings #-}

module Text.Templating.Heist.Splices.Apply where

------------------------------------------------------------------------------
import           Control.Monad.RWS.Strict
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Text.XML.Expat.Tree as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal

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
            st <- get
            processedChildren <- runNodeList $ X.getChildren node
            modify (bindSplice "content" $ return processedChildren)
            maybe (return []) -- TODO: error handling
                  (\(t,ctx) -> do setContext ctx
                                  result <- runNodeList t
                                  put st
                                  return result)
                  (lookupTemplate attr (st {_curContext = nextCtx attr st}))
  where nextCtx name st
            | B.isPrefixOf "/" name = []
            | otherwise             = _curContext st


