{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pops.GA(
  GASolution,
  createMutationOperator,
  createCrossoverOperator,
  createTruncationSelection,
  roulette,
  createTournamentSelection,
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
        let solutionSize = length val
        modVec <- replicateM solutionSize randomGaussian'
        alpha <- randomProbability
        let newVal = zipWith (\s r -> s + r * alpha) val modVec
        return $ updateSolution s newVal
      else
        return s

aritMix :: Double -> (GASolution [Double], GASolution [Double]) -> Rng (GASolution [Double])
aritMix mixProbability (sol1, sol2) = do
  rand <- randomProbability
  if rand < mixProbability then do
    let s1 = value sol1
    let s2 = value sol2
    let solutionSize = length s1
    idx <- randomInt 0 (solutionSize - 1)
    alpha <- randomProbability
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


createTruncationSelection :: Int -> Int -> PopulationalModifier (GASolution [Double])
createTruncationSelection numOfSelected populationSize = truncate
  where
    truncate :: PopulationalModifier (GASolution [Double])
    truncate pop = replicateM populationSize (truncate' pop)
      where
        truncate' pop = do
          let bestCandidates = take numOfSelected $ sortBy (\a b -> compare (fitness a) (fitness b)) pop
          randIndx <- randomInt 0 (numOfSelected - 1)
          return $ bestCandidates!!randIndx


roulette :: PopulationalModifier (GASolution [Double])
roulette pop = replicateM populationSize (roulette' pop)
  where
    populationSize = length pop
    roulette' = do
      let fit = map cost pop
      let cumsum = sum fit
      let weights = map (\x -> 1.0 - x/cumsum) fit
      return $ randomWeightedChoice $ zip pop weights


createTournamentSelection :: Int -> PopulationalModifier (GASolution [Double])
createTournamentSelection tournamentSize = tournament
  where
    tournament :: PopulationalModifier (GASolution [Double])
    tournament pop = replicateM populationSize (tournament' pop)
      where
        populationSize = length pop
        tournament' pop = do
          participants <- replicateM tournamentSize $ sample pop
          return $ last $ scanl1 (\x y -> if cost x < cost y then x else y) participants


getBest :: [GASolution [Double]] -> GASolution [Double]
getBest = minimumBy (\a b -> compare (cost a) (cost b))
