{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}

module Heist.Splices
  ( ifISplice
  , ifCSplice
  , ifElseISplice
  , ifElseCSplice
  , module Heist.Splices.Apply
  , module Heist.Splices.Bind
  , module Heist.Splices.Cache
  , module Heist.Splices.Html
  , module Heist.Splices.Ignore
  , module Heist.Splices.Markdown
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid (Monoid(..))
#endif

import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import           Heist.Splices.Apply
import           Heist.Splices.Bind
import           Heist.Splices.Cache
import           Heist.Splices.Html
import           Heist.Splices.Ignore
import           Heist.Splices.Markdown
import           Heist.Internal.Types.HeistState
import qualified Text.XmlHtml as X

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
ifCSplice :: (Monad n, MonadIO m, MonadBaseControl IO m)
          => (t -> Bool)
          -> RuntimeSplice n t
          -> C.GSplice n m
ifCSplice predicate runtime = do
    chunks <- C.runChildren
    return $ C.yieldRuntime $ do
        a <- runtime
        if predicate a
          then
            C.codeGen chunks
          else
            return mempty


------------------------------------------------------------------------------
-- | Implements an if\/then\/else conditional splice.  It splits its children
-- around the \<else\/\> element to get the markup to be used for the two cases.
ifElseISplice :: Monad m => Bool -> I.Splice m
ifElseISplice cond = getParamNode >>= (rewrite . X.childNodes)
  where
    rewrite nodes = 
      let (ns, ns') = break (\n -> X.tagName n==Just "else") nodes
      in I.runNodeList $ if cond then ns else (drop 1 ns') 


------------------------------------------------------------------------------
-- | Implements an if\/then\/else conditional splice.  It splits its children
-- around the \<else\/\> element to get the markup to be used for the two cases.
ifElseCSplice :: (Monad n, MonadIO m, MonadBaseControl IO m)
              => Bool
              -> C.GSplice n m
ifElseCSplice cond = getParamNode >>= (rewrite . X.childNodes)
  where rewrite nodes = 
          let (ns, ns') = break (\n -> X.tagName n==Just "else") nodes
          in C.runNodeList $ if cond then ns else (drop 1 ns') 
