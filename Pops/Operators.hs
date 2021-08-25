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

data Populational s a where
  PopMod :: (Solution s a) => PopulationalModifier (s a) -> Populational s a -> Populational s a
  Select :: (Solution s a) => Selector (s a) -> Populational s a -> Populational s a -> Populational s a
  IndMod :: (Solution s a) => IndividualModifier (s a) -> Populational s a -> Populational s a
  End :: Populational s a

step :: Solution s a => Populational s a -> [s a] -> Rng [s a]
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

executeAlgorithm :: (Solution s a) => Int -> Int -> Int -> Populational s a -> [s a]
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
