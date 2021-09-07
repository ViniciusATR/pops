{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.Solution (
  Solution (..),
  SimpleSolution (..),
  distance,
  getBest,
  getWorst,
  avgCost
  ) where

import Pops.Rng
import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )

class Solution s where
  cost :: s -> Double
  createRandom :: Rng s

class Solution s => SimpleSolution s where
  getValue :: s -> [Double]
  updateSolution :: s -> [Double] -> s

distance :: SimpleSolution s => s -> s -> Double
distance a b = sqrt $ sum sumOfSquares
  where
    sumOfSquares = zipWith (\x y -> (x - y)**2) (getValue a) (getValue b)

avgCost :: SimpleSolution s => [s] -> Double
avgCost pop =  sum (map cost pop) / n
  where
    n = fromIntegral $ length pop

getBest :: SimpleSolution s => [s] -> s
getBest = minimumBy (\a b -> compare (cost a) (cost b))

getWorst :: SimpleSolution s => [s] -> s
getWorst = maximumBy (\a b -> compare (cost a) (cost b))