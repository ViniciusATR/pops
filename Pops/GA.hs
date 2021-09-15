module Pops.GA(
  createMutationOperator,
  createCrossoverOperator,
  createParCrossoverOperator,
  createTruncationSelection,
  roulette,
  createTournamentSelection,
  getBest
  )where

import Pops.Solution
import Pops.Populational
import Pops.Rng
import Control.Monad.State.Strict
import qualified System.Random.Mersenne.Pure64 as Mer
import Data.List (minimumBy, tails, sortBy)
import Control.DeepSeq (force)
import Control.Parallel.Strategies


createMutationOperator :: SimpleSolution s => Double -> IndividualModifier s
createMutationOperator mutationRate = mutate
  where
    mutate :: SimpleSolution s => IndividualModifier s
    mutate s = do
      rand <- randomProbability
      if rand < mutationRate then do
        let val = getValue s
        let solutionSize = length val
        modVec <- replicateM solutionSize randomGaussian'
        alpha <- randomProbability
        let newVal = zipWith (\s r -> s + r * alpha) val modVec
        return $ updateSolution s newVal
      else
        return s

aritMix :: SimpleSolution s => Double -> (s, s) -> Rng s
aritMix mixProbability (sol1, sol2) = do
  rand <- randomProbability
  if rand < mixProbability then do
    let s1 = getValue sol1
    let s2 = getValue sol2
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

createCrossoverOperator :: SimpleSolution s => Double -> Int -> PopulationalModifier s
createCrossoverOperator mixProbability populationSize = crossover
  where
    crossover :: SimpleSolution s => PopulationalModifier s
    crossover pop = do
      let pairs = take populationSize $ [(a, b) | (a: bs) <- tails pop, b <- bs]
      mapM (aritMix mixProbability) pairs


createParCrossoverOperator :: (NFData s, SimpleSolution s) => Double -> Int -> PopulationalModifier s
createParCrossoverOperator mixProbability populationSize = crossover
  where
    crossover :: (NFData s, SimpleSolution s) => PopulationalModifier s
    crossover pop = do
      let pairs = take populationSize $ [(a, b) | (a: bs) <- tails pop, b <- bs]
      rngMapInParallel' pairs (aritMix mixProbability)


createTruncationSelection :: SimpleSolution s => Int -> Int -> PopulationalModifier s
createTruncationSelection numOfSelected populationSize = truncate
  where
    truncate :: SimpleSolution s => PopulationalModifier s
    truncate pop = replicateM populationSize (truncate' pop)
      where
        truncate' pop = do
          let bestCandidates = take numOfSelected $ sortBy (\a b -> compare (cost a) (cost b)) pop
          randIndx <- randomInt 0 (numOfSelected - 1)
          return $ bestCandidates!!randIndx


roulette :: SimpleSolution s => PopulationalModifier s
roulette pop = replicateM populationSize (roulette' pop)
  where
    populationSize = length pop
    roulette' = do
      let fit = map cost pop
      let cumsum = sum fit
      let weights = map (\x -> 1.0 - x/cumsum) fit
      return $ randomWeightedChoice $ zip pop weights


createTournamentSelection :: SimpleSolution s => Int -> PopulationalModifier s
createTournamentSelection tournamentSize = tournament
  where
    tournament :: SimpleSolution s => PopulationalModifier s
    tournament pop = replicateM populationSize (tournament' pop)
      where
        populationSize = length pop
        tournament' pop = do
          participants <- replicateM tournamentSize $ sample pop
          return $ last $ scanl1 (\x y -> if cost x < cost y then x else y) participants
