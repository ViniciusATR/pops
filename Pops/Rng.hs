module Pops.Rng(
  Rng,
  randomDouble,
  randomProbability,
  randomBool,
  randomInt,
  randomGaussian,
  randomGaussian',
  sample,
  shuffle,
  randomWeightedChoice,
  rngMapInParallel,
  rngMapInParallel'
  )where

import GHC.Word (Word64)
import Data.Bits ((.&.))
import System.Random
import Control.Monad.State.Strict
import qualified Data.Map as M
import Control.DeepSeq (force)
import Control.Parallel.Strategies
import GHC.Conc.Sync (numCapabilities)


type Rng a = State StdGen a

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n l = take n l : splitList n (drop n l)

genSeeds :: Int -> StdGen -> [StdGen]
genSeeds n g = map mkStdGen rs
  where
    rs = force $ evalState (replicateM n $ randomInt 0 9999999) g

rngMapInParallel :: (NFData s) => [s] -> (s -> Rng s) -> Rng [s]
rngMapInParallel ls f = do
  g <- get
  let n = numCapabilities
      groups = splitList n ls
      (gi: gs) = genSeeds (n+1) g
      applyF (xs, gen) = force $ evalState (mapM f xs) gen
      ls' = concat $ parMap rdeepseq applyF $ zip groups gs
  put gi
  return ls'

rngMapInParallel' :: (NFData s) => [(s, s)] -> ((s, s) -> Rng s) -> Rng [s]
rngMapInParallel' ls f = do
  g <- get
  let n = numCapabilities
      groups = splitList n ls
      (gi: gs) = genSeeds (n+1) g
      applyF (xs, gen) = force $ evalState (mapM f xs) gen
      ls' = concat $ parMap rdeepseq applyF $ zip groups gs
  put gi
  return ls'

randomDouble :: Double -> Double -> Rng Double
randomDouble min max = state $ uniformR (min::Double, max::Double)

randomProbability :: Rng Double
randomProbability = randomDouble 0.0 1.0

randomBool :: Rng Bool
randomBool = state $ uniformR (False, True)

randomInt :: Int -> Int -> Rng Int
randomInt min max = state $ uniformR (min::Int, max::Int)

boxMuller :: Floating a => a -> a -> (a, a)
boxMuller u1 u2 = (r * cos t, r * sin t)
  where
    r = sqrt (-2 * log u1)
    t = 2 * pi * u2

gaussian :: Double -> Double -> StdGen -> (Double, StdGen)
gaussian sd m g = (n1 * sd + m, g2)
  where
    (u1, g1) = uniformR (0.0, 1.0) g
    (u2, g2) = uniformR (0.0, 1.0) g1
    (n1, n2) = boxMuller u1 u2

randomGaussian :: Double -> Double -> Rng Double
randomGaussian sd m = state $ gaussian sd m

randomGaussian' :: Rng Double
randomGaussian' = state $ gaussian 1.0 0.0

sample :: [a] -> Rng a
sample xs = do
  idx <- randomInt 0 (n - 1)
  return $ xs !! idx
    where n = length xs

-- Implementação de um shuffle de listas de acordo com
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
-- Obtido também em https://wiki.haskell.org/Random_shuffle

fisherYatesStep :: (M.Map Int a, StdGen ) -> (Int, a) -> (M.Map Int a, StdGen )
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = uniformR (0, i) gen

fisherYates :: [a] -> StdGen -> ([a], StdGen )
fisherYates [] gen = ([], gen)
fisherYates l gen =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen = (M.singleton 0 x, gen)

shuffle :: [a] -> Rng [a]
shuffle xs = state $ fisherYates xs

randomWeightedChoice :: [(a, Double)] -> Rng a
randomWeightedChoice sample = do
  shuffled <- shuffle sample
  let weights = map snd sample
  let minWeight = minimum weights
  let maxWeight = maximum weights
  cutoff <- randomDouble minWeight maxWeight
  let chosen = head $ dropWhile ((<cutoff) . snd) shuffled
  return $ fst chosen
