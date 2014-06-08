{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|

An API implementing a convenient syntax for defining and manipulating splices.
This module was born from the observation that a list of tuples is
semantically ambiguous about how duplicate keys should be handled.
Additionally, the syntax is inherently rather cumbersome and difficult to work
with.  This API takes advantage of do notation to provide a very light syntax
for defining splices while at the same time eliminating the semantic ambiguity
of alists.

Here's how you can define splices:

> mySplices :: Splices Text
> mySplices = do
>   "firstName" ## "John"
>   "lastName"  ## "Smith"

-}

module Heist.SpliceAPI
  {-# DEPRECATED "Use Data.Map.Syntax from the map-syntax package instead" #-}
  ( SplicesM
  , Splices
  , noSplices
  , mapNames
  , prefixSplices
  , namespaceSplices
  , module Data.Map.Syntax
  ) where

import           Control.Applicative
import           Control.Monad.State (State, MonadState, execState, modify)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Map.Syntax
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T



{-# DEPRECATED SplicesM, Splices, noSplices, mapS, applyS, ($$), mapNames
 "Use Data.Map.Syntax from the map-syntax package instead" #-}

type SplicesM s a = MapSyntaxM Text s a

type Splices s = MapSyntax Text s

noSplices :: Splices s
noSplices = mempty


------------------------------------------------------------------------------
-- | Maps a function over all the splices.
mapS :: (a -> b) -> Splices a -> Splices b
mapS = mapV


------------------------------------------------------------------------------
-- | Applies an argument to a splice function.
applyS :: a -> Splices (a -> b) -> Splices b
applyS a = mapS ($a)


------------------------------------------------------------------------------
-- | Infix operator for @flip applyS@
($$) :: Splices (a -> b) -> a -> Splices b
($$) = flip applyS
infixr 0 $$


------------------------------------------------------------------------------
-- | Maps a function over all the splice names.
mapNames :: (Text -> Text) -> Splices a -> Splices a
mapNames = mapK


------------------------------------------------------------------------------
-- | Adds a prefix to the tag names for a list of splices.  If the existing
-- tag name is empty, then the new tag name is just the prefix.  Otherwise the
-- new tag name is the prefix followed by the separator followed by the
-- existing name.
prefixSplices :: Text -> Text -> Splices a -> Splices a
prefixSplices sep pre = mapK f
  where
    f t = if T.null t then pre else T.concat [pre,sep,t]


------------------------------------------------------------------------------
-- | 'prefixSplices' specialized to use a colon as separator in the style of
-- XML namespaces.
namespaceSplices :: Text -> Splices a -> Splices a
namespaceSplices = prefixSplices ":"

