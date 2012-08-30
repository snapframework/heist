module Heist.Splices.Html where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Interpreted.Internal
import           Heist.Types


------------------------------------------------------------------------------
-- | Name for the html splice.
htmlTag :: Text
htmlTag = "html"


------------------------------------------------------------------------------
-- | The html splice runs all children and then traverses the returned node
-- forest removing all head nodes.  Then it merges them all and prepends it to
-- the html tag's child list.
htmlImpl :: Monad n => Splice n
htmlImpl = do
    node <- getParamNode
    children <- runNodeList $ X.childNodes node
    let (heads, mnode) = extractHeads $ node { X.elementChildren = children }
        new (X.Element t a c) = X.Element t a $
            X.Element "head" [] heads : c
        new n = n
    stopRecursion
    return [maybe node new mnode]

------------------------------------------------------------------------------
-- | Extracts all heads from a node tree.
extractHeads :: X.Node
             -- ^ The root (html) node
             -> ([X.Node], Maybe X.Node)
             -- ^ A tuple of a list of head nodes and the original tree with
             --   heads removed.
extractHeads (X.Element t a c)
  | t == "head" = (c, Nothing)
  | otherwise   = (concat heads, Just $ X.Element t a (catMaybes mcs))
  where
    (heads, mcs) = unzip $ map extractHeads c
extractHeads n = ([], Just n)

