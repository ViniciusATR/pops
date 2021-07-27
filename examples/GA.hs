{-# LANGUAGE FlexibleInstances #-}

import Pops.Data
import Pops.Operators
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Data.List (minimumBy, tails, sortBy)


instance Representation [Double] where
  cost s =  10 * n + cumsum
    where
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

data GASolution a = GASolution {
                                  value :: a,
                                  fitness :: Double
                               } deriving(Show)

instance Solution (GASolution [Double]) where
  createRandom = do
    vec <- replicateM 2 $ randomDouble (-5.12) 5.12
    return $ GASolution vec (cost vec)

mutate :: IndividualModifier (GASolution [Double])
mutate s = do
  rand <- randomProbability
  if rand < 0.5 then do
    let val = value s
    modVec <- replicateM 2 randomGaussian'
    alpha <- randomProbability
    let newVal = zipWith (\s r -> s + r * alpha) val modVec
    return $ GASolution newVal (cost newVal)
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
    return $ GASolution newVal (cost newVal)
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
getBest = minimumBy (\a b -> compare (cost $ value a) (cost $ value b))

ga = PopMod elitist (PopMod crossover (IndMod mutate End))

main :: IO ()
main = do
  let pops = executeAlgorithm 42 1000 20 ga
  print $ getBest pops
