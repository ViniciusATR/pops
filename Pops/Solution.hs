{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.Solution (
  Solution (..),
  SimpleSolution (..)
  ) where

import Pops.Rng

class Solution s where
  cost :: s -> Double
  createRandom :: Rng s

class Solution s => SimpleSolution s where
  getValue :: s -> [Double]
  updateSolution :: s -> [Double] -> s

