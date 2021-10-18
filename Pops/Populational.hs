{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Pops.Populational (
  IndividualModifier,
  PopulationalModifier,
  Selector,
  Populational(..),
  executeAlgorithm,
  parExecuteAlgorithm
 )where
import Pops.Solution
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Control.Parallel.Strategies


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


simplify :: Populational s -> Populational s
simplify End = End

simplify (Select sel algo1 algo2) = Select sel (simplify algo1) (simplify algo2)

simplify (PopMod mod pop) =
  case pop of
    PopMod _ _ -> let PopMod mod' pop' = simplify pop
                  in PopMod (mod >=> mod') pop'
    _          -> PopMod mod (simplify pop)

simplify (IndMod mod pop) =
  case pop of
    IndMod _ _ -> let IndMod mod' pop' = simplify pop
                  in IndMod (mod >=> mod') pop'
    _          -> IndMod mod (simplify pop)

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


parStep :: (Solution s, NFData s) => Populational s -> [s] -> Rng [s]
parStep End pop = return pop

parStep (IndMod mod next) pop = do
  pop' <- rngMapInParallel pop mod
  step next pop'

parStep (PopMod mod next) pop = do
  pop'<- mod pop
  step next pop'

parStep (Select sel op1 op2) pop = do
  pop1 <- step op1 pop
  pop2 <- step op2 pop
  sel pop1 pop2

executeAlgorithm :: (Solution s) => Int -> Int -> Int -> Populational s -> [s]
executeAlgorithm seed size iter algo = evalState (exec algo initPop 0) gen
  where
    algo' = simplify algo
    gen = mkStdGen seed
    initPop = evalState (replicateM size createRandom) gen
    exec algo pop currIter
      | iter <= currIter = return pop
      | otherwise = do
          pop' <- step algo pop
          let nextIter = currIter + 1
          exec algo pop' nextIter

parExecuteAlgorithm :: (Solution s, NFData s) => Int -> Int -> Int -> Populational s -> [s]
parExecuteAlgorithm seed size iter algo = evalState (exec algo initPop 0) gen
  where
    algo' = simplify algo
    gen = mkStdGen seed
    initPop = evalState (replicateM size createRandom) gen
    exec algo pop currIter
      | iter <= currIter = return pop
      | otherwise = do
          pop' <- parStep algo pop
          let nextIter = currIter + 1
          exec algo pop' nextIter
