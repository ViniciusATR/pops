{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.PSO (
  PSOSolution,
  createChangeVelocityOperator,
  updatePosition,
  updateBestPosition,
  getBestPosition
  )where

import Pops.Data
import Pops.Operators
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy)


data PSOSolution a = PSOSolution {
                                  value :: a,
                                  velocity :: a,
                                  bestPosition :: a,
                                  fitness :: Double
                               } deriving(Show)

instance Solution PSOSolution [Double] where
  cost sol =  10 * n + cumsum
    where
      s = value sol
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

  updateSolution current newValue = intermediate { fitness = fitness' }
    where
      intermediate = current { value = newValue }
      fitness' = cost intermediate

  createRandom = do
    pos <- replicateM n $ randomDouble (-dim) dim
    vel <- replicateM n $ randomDouble (-maxv) maxv
    let newSol = PSOSolution pos vel pos 0.0
    return $ updateSolution newSol pos
      where
        n = 2
        dim = 5.12
        maxv = 1.0

validateVelocity :: Double -> Double -> Double
validateVelocity max vel
  | vel > max = max
  | vel < -max = -max
  | otherwise = vel

getBestPosition :: [PSOSolution [Double]] -> PSOSolution [Double]
getBestPosition = minimumBy (\a b -> compare (cost a) (cost b))

updateBestPosition :: IndividualModifier (PSOSolution [Double])
updateBestPosition sol = do
  if currentCost > bestCost
     then return $ sol {bestPosition = value sol}
     else return sol
  where
    bestCost = cost $ sol { value = bestPosition sol }
    currentCost = fitness sol

updatePosition :: IndividualModifier (PSOSolution [Double])
updatePosition sol = return $ updateSolution sol newValue
  where newValue = zipWith (+) (value sol) (velocity sol)

createChangeVelocityOperator :: Double -> Double -> Double -> PopulationalModifier (PSOSolution [Double])
createChangeVelocityOperator maximumVelocity localbias globalbias = changeVelocity
  where
    changeVelocity :: PopulationalModifier (PSOSolution [Double])
    changeVelocity pop = mapM (modify gbest) pop
      where
        gbest = value $ getBestPosition pop

    modify :: [Double] -> PSOSolution [Double] -> Rng (PSOSolution [Double])
    modify gb sol = do
      let c = value sol
      let cv = velocity sol
      let lb = bestPosition sol
      r1 <- randomProbability
      r2 <- randomProbability
      let globalcoeff = zipWith (\x y -> globalbias * r1 * (y - x)) c gb
      let localcoeff = zipWith (\x y -> localbias * r2 * (y - x)) c lb
      let newVelocity = map (validateVelocity maximumVelocity) $ zipWith3 (\a b c -> a + b + c) cv globalcoeff localcoeff
      return $ sol { velocity = newVelocity }
