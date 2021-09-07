{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.PSO (
  createChangeVelocityOperator,
  updatePosition,
  updateBestPosition,
  SolutionWithHistory(..),
  SolutionWithVelocity(..)
  )where

import Pops.Solution
import Pops.Populational
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy)

class SimpleSolution s => SolutionWithHistory s where
  getBestPosition :: s -> [Double]
  updateBest :: s -> [Double] -> s

class SimpleSolution s => SolutionWithVelocity s where
  getVelocity :: s -> [Double]
  updateVelocity :: s -> [Double] -> s


validateVelocity :: Double -> Double -> Double
validateVelocity max vel
  | vel > max = max
  | vel < -max = -max
  | otherwise = vel


updateBestPosition :: (SolutionWithVelocity s, SolutionWithHistory s) => IndividualModifier s
updateBestPosition sol = do
  if currentCost > bestCost
     then return $ updateBest sol $ getValue sol
     else return sol
  where
    bestCost = cost $ updateSolution sol $ getBestPosition sol
    currentCost = cost sol

updatePosition :: SolutionWithVelocity s =>  IndividualModifier s
updatePosition sol = return $ updateSolution sol newValue
  where newValue = zipWith (+) (getValue sol) (getVelocity sol)

createChangeVelocityOperator :: (SolutionWithVelocity s, SolutionWithHistory s) =>  Double -> Double -> Double -> PopulationalModifier s
createChangeVelocityOperator maximumVelocity localbias globalbias = changeVelocity
  where
    changeVelocity :: (SolutionWithVelocity s, SolutionWithHistory s) => PopulationalModifier s
    changeVelocity pop = mapM (modify gbest) pop
      where
        gbest = getValue $ getBest pop

    modify :: (SolutionWithVelocity s, SolutionWithHistory s) => [Double] -> s -> Rng s
    modify gb sol = do
      let c = getValue sol
      let cv = getVelocity sol
      let lb = getBestPosition sol
      r1 <- randomProbability
      r2 <- randomProbability
      let globalcoeff = zipWith (\x y -> globalbias * r1 * (y - x)) c gb
      let localcoeff = zipWith (\x y -> localbias * r2 * (y - x)) c lb
      let newVelocity = map (validateVelocity maximumVelocity) $ zipWith3 (\a b c -> a + b + c) cv globalcoeff localcoeff
      return $ updateVelocity sol newVelocity
