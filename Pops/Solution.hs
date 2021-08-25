{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.Solution (
  Solution (..),
  ) where

import Pops.Rng

class Solution s a where
  cost :: s a -> Double
  updateSolution :: s a -> a -> s a
  createRandom :: Rng (s a)
