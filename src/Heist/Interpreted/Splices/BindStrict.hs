module Heist.Interpreted.Splices.BindStrict where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Interpreted.Internal
import           Heist.Interpreted.Splices.Apply
import           Heist.Interpreted.Splices.Bind
import           Heist.Types

-- | Default name for the bind splice.
bindStrictTag :: Text
bindStrictTag = "bindStrict"


------------------------------------------------------------------------------
-- | Implementation of the bind splice.
bindStrictImpl :: Monad n => Splice n
bindStrictImpl = do
    node <- getParamNode
    cs <- runChildren
    let err = "must supply \"" ++ T.unpack bindAttr ++
              "\" attribute in <" ++ T.unpack (X.elementTag node) ++ ">"
    maybe (return () `orError` err) (add cs)
          (X.getAttribute bindAttr node)
    return []

  where
    add cs nm = modifyTS $ bindSplice nm $ do
        caller <- getParamNode
        ctx <- getContext
        rawApply cs ctx (X.childNodes caller)
