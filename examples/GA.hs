{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Pops.Solution
import Pops.GA
import Pops.Operators
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Data.List (minimumBy, tails, sortBy)


mutate = createMutationOperator 0.5
crossover = createCrossoverOperator 0.5 1000
truncateSelect = createTruncationSelection 10 1000

ga = PopMod truncateSelect (PopMod crossover (IndMod mutate End))

main :: IO ()
main = do
  let pops = executeAlgorithm 42 1000 20 ga
  print $ getBest pops
