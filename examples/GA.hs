{-# LANGUAGE FlexibleInstances #-}

import Pops.Data
import Pops.Operators
import System.Random
import Control.Monad.State.Strict
import Data.List (minimumBy, tails, sortBy)


randomBool :: Rng Bool
randomBool = state $ randomR (True, False)

randomInt :: Int -> Int -> Rng Int
randomInt min max = state $ randomR (min::Int, max::Int)

randomDouble :: Double -> Double -> Rng Double
randomDouble min max = state $ randomR (min::Double, max::Double)

randomProbability :: Rng Double
randomProbability = state $ randomR (0.0::Double, 1.0::Double)

boxMuller :: Floating a => a -> a -> (a, a)
boxMuller u1 u2 = (r * cos t, r * sin t)
  where
    r = sqrt (-2 * log u1)
    t = 2 * pi * u2

gaussian :: (RandomGen g, Random a, Floating a) => a -> a -> g -> (a, g)
gaussian sd m g = (n1 * sd + m, g2)
  where
    (u1, g1) = randomR (0, 1) g
    (u2, g2) = randomR (0, 1) g1
    (n1, n2) = boxMuller u1 u2

randomGaussian :: (Floating a, Random a) => a -> a -> Rng a
randomGaussian sd m = state $ gaussian sd m

randomGaussian' :: (Floating a, Random a) => Rng a
randomGaussian' = state $ gaussian 1.0 0.0

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
