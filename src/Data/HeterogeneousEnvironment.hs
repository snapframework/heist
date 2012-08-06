{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

------------------------------------------------------------------------------
module Data.HeterogeneousEnvironment
  ( KeyGen
  , HeterogeneousEnvironment
  , Key
  , newKeyGen
  , empty
  , makeKey
  , lookup
  , insert
  , delete
  , adjust
  , getKeyId
  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.IntMap   (IntMap)
import qualified Data.IntMap   as IM
import           Data.IORef
import           GHC.Exts
import           Prelude hiding (lookup)
import           Unsafe.Coerce

------------------------------------------------------------------------------
data HeterogeneousEnvironment = HeterogeneousEnvironment (IntMap Any)
newtype Key a = Key Int
newtype KeyGen = KeyGen (IORef Int)


------------------------------------------------------------------------------
-- | If you use two different KeyGens to work with the same map, you deserve
-- what you get.
newKeyGen :: IO KeyGen
newKeyGen = liftM KeyGen $ newIORef 0


------------------------------------------------------------------------------
getKeyId :: Key a -> Int
getKeyId (Key x) = x


------------------------------------------------------------------------------
empty :: HeterogeneousEnvironment
empty = HeterogeneousEnvironment $ IM.empty


------------------------------------------------------------------------------
makeKey :: KeyGen -> IO (Key a)
makeKey (KeyGen gen) = do
    k <- atomicModifyIORef gen nextKey
    return $ Key k
  where
    nextKey !x = if x >= maxBound-1
                   then error "too many keys generated"
                   else let !x' = x+1 in (x',x)


------------------------------------------------------------------------------
lookup :: Key a -> HeterogeneousEnvironment -> Maybe a
lookup (Key k) (HeterogeneousEnvironment m) = fmap unsafeCoerce $ IM.lookup k m


------------------------------------------------------------------------------
insert :: Key a -> a -> HeterogeneousEnvironment -> HeterogeneousEnvironment
insert (Key k) v (HeterogeneousEnvironment m) = HeterogeneousEnvironment $
                                                IM.insert k (unsafeCoerce v) m


------------------------------------------------------------------------------
delete :: Key a -> HeterogeneousEnvironment -> HeterogeneousEnvironment
delete (Key k) (HeterogeneousEnvironment m) = HeterogeneousEnvironment $
                                              IM.delete k m


------------------------------------------------------------------------------
adjust :: (a -> a) -> Key a -> HeterogeneousEnvironment -> HeterogeneousEnvironment
adjust f (Key k) (HeterogeneousEnvironment m) = HeterogeneousEnvironment $
                                                IM.adjust f' k m
  where
    f' = unsafeCoerce . f . unsafeCoerce
