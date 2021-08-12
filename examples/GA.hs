{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

mutate :: IndividualModifier (GASolution [Double])
mutate s = do
  rand <- randomProbability
  if rand < 0.5 then do
    let val = value s
    modVec <- replicateM 2 randomGaussian'
    alpha <- randomProbability
    let newVal = zipWith (\s r -> s + r * alpha) val modVec
    return $ updateSolution s newVal
  else
    return s

aritMix :: (GASolution [Double], GASolution [Double]) -> Rng (GASolution [Double])
aritMix (sol1, sol2) = do
  rand <- randomProbability
  if rand < 0.5 then do
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

crossover :: PopulationalModifier (GASolution [Double])
crossover pop = do
  let pairs = take 1000 $ [(a, b) | (a: bs) <- tails pop, b <- bs]
  mapM aritMix pairs


elitist :: PopulationalModifier (GASolution [Double])
elitist pop = replicateM 1000 (elitist' pop)
  where
    elitist' pop = do
      let bestCandidates = take 100 $ sortBy (\a b -> compare (fitness a) (fitness b)) pop
      randIndx <- randomInt 0 99
      return $ bestCandidates!!randIndx

getBest :: [GASolution [Double]] -> GASolution [Double]
getBest = minimumBy (\a b -> compare (cost a) (cost b))

ga = PopMod elitist (PopMod crossover (IndMod mutate End))

main :: IO ()
main = do
  let pops = executeAlgorithm 42 1000 20 ga
  print $ getBest pops
