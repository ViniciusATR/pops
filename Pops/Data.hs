{-# LANGUAGE FlexibleInstances #-}
module Pops.Data (
  Rng,
  Representation (..),
  Solution (..),
  ) where

import System.Random
import Control.Monad.State.Strict

type Rng a = State StdGen a

class Representation a where
  cost :: a -> Double

class Solution s where
  createRandom :: Rng s
