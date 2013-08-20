{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}

module Heist.SpliceAPI where

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T


------------------------------------------------------------------------------
-- | A monad providing convenient syntax for defining splices.
newtype SplicesM s a = SplicesM { unSplices :: State (Map Text s) a }
  deriving (Monad, MonadState (Map Text s))


type Splices s = SplicesM s ()


noSplices :: Splices s
noSplices = put M.empty


------------------------------------------------------------------------------
-- | Runs the SplicesM monad, generating a map of splices.
runSplices :: SplicesM s a -> Map Text s
runSplices splices = execState (unSplices splices) M.empty


splicesToList :: SplicesM s a -> [(Text, s)]
splicesToList = M.toList . runSplices


mapS :: (a -> b) -> Splices a -> Splices b
mapS f ss = put $ M.map f $ runSplices ss 


applyS :: a -> Splices (a -> b) -> Splices b
applyS a = mapS ($a)


insert :: Text -> s -> Splices s -> Splices s
insert = insertWith const

insertWith :: (s -> s -> s) -> Text -> s -> SplicesM s a2 -> SplicesM s ()
insertWith f k v b = put $ M.insertWith f k v (runSplices b)


unionWith :: (s -> s -> s) -> SplicesM s a1 -> SplicesM s a2 -> SplicesM s ()
unionWith f a b = put $ M.unionWith f (runSplices a) (runSplices b)


($$) :: Splices (a -> b) -> a -> Splices b
($$) = flip applyS
infixr 0 $$


------------------------------------------------------------------------------
-- | Inserts into the map only if the key does not exist.
(?) :: Text -> s -> Splices s
(?) tag splice = modify $ M.insertWith (const id) tag splice
infixr 0 ?


------------------------------------------------------------------------------
-- | Forces an insert into the map.  If the key already exists, its value is
-- overwritten.
(?!) :: Text -> s -> Splices s
(?!) tag splice = modify $ M.insert tag splice
infixr 0 ?!


mapNames :: (Text -> Text) -> Splices a -> Splices a
mapNames f = put . M.mapKeys f . runSplices


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

