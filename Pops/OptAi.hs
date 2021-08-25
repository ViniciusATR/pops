module Pops.OptAi (
  createClonalSelection,
  createTrimmingOperator,
  getBest,
  getWorst,
  NormalizedSolution(..)
  )where

import Pops.Solution
import Pops.Operators
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )


class SimpleSolution s => NormalizedSolution s where
  getNormalizedFitness :: s -> Double
  updateNormalizedFitness :: [s] -> [s]

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

mutate :: NormalizedSolution s => Double -> s -> Rng s
mutate beta sol = do
  modVec <- replicateM 2 randomGaussian'
  let alpha = (1.0/beta) * exp ( - (getNormalizedFitness sol) )
  let newVec = zipWith (\s r -> s + r * alpha) (getValue sol) modVec
  return $ updateSolution sol newVec


createClonalSelection :: NormalizedSolution s => Double -> Int -> PopulationalModifier s
createClonalSelection beta numberOfClones = cloneSelection
  where
    cloneSelection :: NormalizedSolution s => PopulationalModifier s
    cloneSelection pop = do
      let pop' = updateNormalizedFitness pop
      mapM individualClone pop'
      where
        individualClone sol = do
          mutatedClones <- replicateM numberOfClones (mutate beta sol)
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
