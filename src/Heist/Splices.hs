module Heist.Splices
  ( ifISplice
  , ifCSplice
  , module Heist.Splices.Apply
  , module Heist.Splices.Bind
  , module Heist.Splices.Cache
  , module Heist.Splices.Html
  , module Heist.Splices.Ignore
  , module Heist.Splices.Markdown
  ) where

import           Data.Monoid
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import           Heist.Splices.Apply
import           Heist.Splices.Bind
import           Heist.Splices.Cache
import           Heist.Splices.Html
import           Heist.Splices.Ignore
import           Heist.Splices.Markdown

------------------------------------------------------------------------------
-- | Run the splice contents if given condition is True, make splice disappear
-- if not.
ifISplice :: Monad m => Bool -> I.Splice m
ifISplice cond =
    case cond of
      False -> return []
      True -> I.runChildren


------------------------------------------------------------------------------
-- | Function for constructing if splices that use a runtime predicate
-- function to determine whether the node's children should be rendered.
ifCSplice :: Monad m
          => (t -> Bool)
          -> C.Promise t
          -> C.Splice m
ifCSplice predicate prom = do
    chunks <- C.runChildren
    return $ C.yieldRuntime $ do
        a <- C.getPromise prom
        if predicate a
          then
            C.codeGen chunks
          else
            return mempty

