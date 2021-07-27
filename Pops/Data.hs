{-# LANGUAGE FlexibleInstances #-}
module Pops.Data (
  Representation (..),
  Solution (..),
  ) where

import Pops.Rng

class Representation a where
  cost :: a -> Double

class Solution s where
  createRandom :: Rng s
