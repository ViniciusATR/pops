{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.GA(
  GASolution,
  createMutationOperator,
  createCrossoverOperator,
  createElitistSelector,
  getBest
  )where

import Pops.Data
import Pops.Operators
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Data.List (minimumBy, tails, sortBy)


data GASolution a = GASolution {
                                  value :: a,
                                  fitness :: Double
                               } deriving(Show)

instance Solution GASolution [Double] where
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
    let newSol = GASolution [0.0, 0.0] 0.0
    vec <- replicateM 2 $ randomDouble (-5.12) 5.12
    return $ updateSolution newSol vec

createMutationOperator :: Double -> IndividualModifier (GASolution [Double])
createMutationOperator mutationRate = mutate
  where
    mutate :: IndividualModifier (GASolution [Double])
    mutate s = do
      rand <- randomProbability
      if rand < mutationRate then do
        let val = value s
        modVec <- replicateM 2 randomGaussian'
        alpha <- randomProbability
        let newVal = zipWith (\s r -> s + r * alpha) val modVec
        return $ updateSolution s newVal
      else
        return s

aritMix :: Double -> (GASolution [Double], GASolution [Double]) -> Rng (GASolution [Double])
aritMix mixProbability (sol1, sol2) = do
  rand <- randomProbability
  if rand < mixProbability then do
    idx <- randomInt 0 1
    alpha <- randomProbability
    let s1 = value sol1
    let s2 = value sol2
    let firstSection = take ( idx + 1 ) s1
    let s1' = drop (idx + 1) s1
    let s2' = drop (idx + 1) s2
    let secondSection = zipWith (\x y -> alpha * x + (1 - alpha) * y) s1' s2'
    let newVal = firstSection ++ secondSection
    return $ updateSolution sol1 newVal
  else do
    chooseRand <- randomBool
    return $ if chooseRand then sol1 else sol2

createCrossoverOperator :: Double -> Int -> PopulationalModifier (GASolution [Double])
createCrossoverOperator mixProbability populationSize = crossover
  where
    crossover :: PopulationalModifier (GASolution [Double])
    crossover pop = do
      let pairs = take populationSize $ [(a, b) | (a: bs) <- tails pop, b <- bs]
      mapM (aritMix mixProbability) pairs


createElitistSelector :: Int -> Int -> PopulationalModifier (GASolution [Double])
createElitistSelector numOfSelected populationSize = elitist
  where
    elitist :: PopulationalModifier (GASolution [Double])
    elitist pop = replicateM populationSize (elitist' pop)
      where
        elitist' pop = do
          let bestCandidates = take numOfSelected $ sortBy (\a b -> compare (fitness a) (fitness b)) pop
          randIndx <- randomInt 0 (numOfSelected - 1)
          return $ bestCandidates!!randIndx

getBest :: [GASolution [Double]] -> GASolution [Double]
getBest = minimumBy (\a b -> compare (cost a) (cost b))
