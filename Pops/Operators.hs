{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Pops.Operators (
  IndividualModifier,
  PopulationalModifier,
  Selector,
  Populational(..),
  executeAlgorithm
 )where
import Pops.Solution
import Pops.Rng
import System.Random
import Control.Monad.State.Strict

-- A modification localized to a single solution
-- It's logic does not need context contained in the whole population
-- like the current best solution.
type IndividualModifier a = a -> Rng a
-- A modification that needs context from the whole population
-- such as the mean cost or the best current solution.
type PopulationalModifier a = [a] -> Rng [a]
-- An operation of choice between two operations.
type Selector a = [a] -> [a] -> Rng [a]

data Populational s where
  PopMod :: (Solution s) => PopulationalModifier s -> Populational s -> Populational s
  Select :: (Solution s) => Selector s -> Populational s -> Populational s -> Populational s
  IndMod :: (Solution s) => IndividualModifier s -> Populational s -> Populational s
  End :: Populational s

step :: Solution s => Populational s -> [s] -> Rng [s]
step End pop = return pop

step (IndMod mod next) pop = do
  pop' <- mapM mod pop
  step next pop'

step (PopMod mod next) pop = do
  pop'<- mod pop
  step next pop'

step (Select sel op1 op2) pop = do
  pop1 <- step op1 pop
  pop2 <- step op2 pop
  sel pop1 pop2

executeAlgorithm :: (Solution s) => Int -> Int -> Int -> Populational s -> [s]
executeAlgorithm seed size iter algo = evalState (exec algo initPop 0) gen
  where
    gen = mkStdGen seed
    initPop = evalState (replicateM size createRandom) gen
    exec algo pop currIter
      | iter <= currIter = return pop
      | otherwise = do
          pop' <- step algo pop
          let nextIter = currIter + 1
          exec algo pop' nextIter
