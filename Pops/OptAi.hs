module Pops.OptAi (
  createClonalSelection,
  createParClonalSelection,
  createTrimmingOperator,
  NormalizedSolution(..)
  )where

import Pops.Solution
import Pops.Populational
import Pops.Rng
import Control.Monad.State.Strict ( replicateM )
import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )
import Control.DeepSeq (force)
import Control.Parallel.Strategies


class SimpleSolution s => NormalizedSolution s where
  getNormalizedFitness :: s -> Double
  updateNormalizedFitness :: [s] -> [s]


mutate :: NormalizedSolution s => Int -> Double -> s -> Rng s
mutate size beta sol = do
  modVec <- replicateM size randomGaussian'
  let alpha = (1.0/beta) * exp ( - (getNormalizedFitness sol) )
  let newVec = zipWith (\s r -> s + r * alpha) (getValue sol) modVec
  return $ updateSolution sol newVec


createClonalSelection :: NormalizedSolution s => Double -> Int -> Int -> PopulationalModifier s
createClonalSelection beta size numberOfClones = cloneSelection
  where
    cloneSelection :: NormalizedSolution s => PopulationalModifier s
    cloneSelection pop = do
      let pop' = updateNormalizedFitness pop
      mapM individualClone pop'
      where
        individualClone sol = do
          mutatedClones <- replicateM numberOfClones (mutate size beta sol)
          return $ getBest $ mutatedClones ++ [sol]

createParClonalSelection :: (NFData s, NormalizedSolution s) => Double -> Int -> Int -> PopulationalModifier s
createParClonalSelection beta size numberOfClones = cloneSelection
  where
    cloneSelection :: (NFData s, NormalizedSolution s) => PopulationalModifier s
    cloneSelection pop = do
      let pop' = updateNormalizedFitness pop
      rngMapInParallel pop' individualClone
      where
        individualClone sol = do
          mutatedClones <- replicateM numberOfClones (mutate size beta sol)
          return $ getBest $ mutatedClones ++ [sol]

trim :: SimpleSolution s => Double -> Int -> [s] ->  Rng [s]
trim distanceCoefficient numberOfAdditions pop = do
  let neighbours = groupBy (\a b -> distance a b <= distanceCoefficient) pop
  trimmed <- mapM sample neighbours
  newCells <- replicateM numberOfAdditions createRandom
  return $ trimmed ++ newCells

createTrimmingOperator :: SimpleSolution s =>  Double -> Int -> Selector s
createTrimmingOperator distanceCoefficient numberOfAdditions = triggerTrim
  where
    triggerTrim :: SimpleSolution s => Selector s
    triggerTrim old new = do
      let currAvg = avgCost old
      let newAvg = avgCost new
      if newAvg < currAvg then do
        return new
      else do
        trim distanceCoefficient numberOfAdditions new
