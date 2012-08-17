{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Heist.Compiled.Splices.Bind where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import qualified Data.DList                      as DL
import           Data.Text (Text)
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Compiled.Internal
import           Heist.Compiled.Splices.Apply
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
bindImpl :: (Monad m) => Splice m
bindImpl = do
    node <- getParamNode
    maybe (return ())
          (add node)
          (X.getAttribute bindAttr node)
    return DL.empty

  where
    add node nm = modifyTS $ bindSplice nm $ do
        caller <- getParamNode
        ctx <- getContext
        rawApply (X.childNodes node) ctx (X.childNodes caller)


