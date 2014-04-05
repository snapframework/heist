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

module Heist.SpliceAPI where

import           Control.Applicative (Applicative)
import           Control.Monad.State (MonadState, State, execState, modify)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid         (Monoid (..))
import           Data.Text           (Text)
import qualified Data.Text           as T


------------------------------------------------------------------------------
-- | A monad providing convenient syntax for defining splices.
newtype SplicesM s a = SplicesM { unSplices :: State (Map Text s) a }
  deriving (Functor, Applicative, Monad, MonadState (Map Text s))


------------------------------------------------------------------------------
-- | Monoid instance does a union of the two maps with the second map
-- overwriting any duplicates.
instance Monoid (Splices s) where
  mempty = noSplices
  mappend = unionWithS (\_ b -> b)


------------------------------------------------------------------------------
-- | Convenient type alias that will probably be used most of the time.
type Splices s = SplicesM s ()


------------------------------------------------------------------------------
-- | Forces a splice to be added.  If the key already exists, its value is
-- overwritten.
(##) :: Text -> s -> Splices s
(##) tag splice = modify $ M.insert tag splice
infixr 0 ##


------------------------------------------------------------------------------
-- | Tries to add a splice, but if the key already exists, then it throws an
-- error message.  This may be useful if name collisions are bad and you want
-- to crash when they occur.
(#!) :: Text -> s -> Splices s
(#!) tag splice = modify $ M.insertWithKey err tag splice
  where
    err k _ _ = error $ "Key "++show k++" already exists in the splice map"
infixr 0 #!


------------------------------------------------------------------------------
-- | Inserts into the map only if the key does not already exist.
(#?) :: Text -> s -> Splices s
(#?) tag splice = modify $ M.insertWith (const id) tag splice
infixr 0 #?


------------------------------------------------------------------------------
-- | A `Splices` with nothing in it.
noSplices :: Splices s
noSplices = return ()


------------------------------------------------------------------------------
-- | Runs the SplicesM monad, generating a map of splices.
runSplices :: SplicesM s a -> Map Text s
runSplices splices = execState (unSplices splices) M.empty


------------------------------------------------------------------------------
-- | Constructs an alist representation.
splicesToList :: SplicesM s a -> [(Text, s)]
splicesToList = M.toList . runSplices


------------------------------------------------------------------------------
-- | Internal helper function for adding a map.
add :: Map Text s -> Splices s
add m = modify (\s -> M.unionWith (\_ b -> b) s m)


------------------------------------------------------------------------------
-- | Maps a function over all the splices.
mapS :: (a -> b) -> Splices a -> Splices b
mapS f ss = add $ M.map f $ runSplices ss


------------------------------------------------------------------------------
-- | Applies an argument to a splice function.
applyS :: a -> Splices (a -> b) -> Splices b
applyS a = mapS ($a)


------------------------------------------------------------------------------
-- | Inserts a splice into the 'Splices'.
insertS :: Text -> s -> Splices s -> Splices s
insertS = insertWithS const


------------------------------------------------------------------------------
-- | Inserts a splice with a function combining new value and old value.
insertWithS :: (s -> s -> s) -> Text -> s -> SplicesM s a2 -> SplicesM s ()
insertWithS f k v b = add $ M.insertWith f k v (runSplices b)


------------------------------------------------------------------------------
-- | Union of `Splices` with a combining function.
unionWithS :: (s -> s -> s) -> SplicesM s a1 -> SplicesM s a2 -> SplicesM s ()
unionWithS f a b = add $ M.unionWith f (runSplices a) (runSplices b)


------------------------------------------------------------------------------
-- | Infix operator for @flip applyS@
($$) :: Splices (a -> b) -> a -> Splices b
($$) = flip applyS
infixr 0 $$


------------------------------------------------------------------------------
-- | Maps a function over all the splice names.
mapNames :: (Text -> Text) -> Splices a -> Splices a
mapNames f = add . M.mapKeys f . runSplices


-- The following two functions are formulated as functions of Splices instead
-- of a single splice because they operate on the splice names instead of the
-- splices themselves.


------------------------------------------------------------------------------
-- | Adds a prefix to the tag names for a list of splices.  If the existing
-- tag name is empty, then the new tag name is just the prefix.  Otherwise the
-- new tag name is the prefix followed by the separator followed by the
-- existing name.
prefixSplices :: Text -> Text -> Splices a -> Splices a
prefixSplices sep pre = mapNames f
  where
    f t = if T.null t then pre else T.concat [pre,sep,t]


------------------------------------------------------------------------------
-- | 'prefixSplices' specialized to use a colon as separator in the style of
-- XML namespaces.
namespaceSplices :: Text -> Splices a -> Splices a
namespaceSplices = prefixSplices ":"

