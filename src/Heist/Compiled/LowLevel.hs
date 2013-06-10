{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Heist.Compiled.LowLevel
  (
  -- * Lower level promise functions
    Promise
  , newEmptyPromise
  , getPromise
  , putPromise
  , adjustPromise

  ) where

import           Heist.Compiled.Internal
