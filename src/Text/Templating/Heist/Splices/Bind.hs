{-# LANGUAGE OverloadedStrings #-}

module Text.Templating.Heist.Splices.Bind where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import qualified Text.XML.Expat.Tree as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types

-- | Default name for the bind splice.
bindTag :: ByteString
bindTag = "bind"


------------------------------------------------------------------------------
-- | Default attribute name for the bind tag.
bindAttr :: ByteString
bindAttr = "tag"


------------------------------------------------------------------------------
-- | Implementation of the bind splice.
bindImpl :: Monad m => Splice m
bindImpl = do
    node <- getParamNode
    maybe (return ())
          (add node)
          (X.getAttribute node bindAttr)
    return []

  where
    add node nm = modifyTS $ bindSplice nm (return $ X.getChildren node)


