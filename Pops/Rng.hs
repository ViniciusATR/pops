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
  splitList,
  rngMapInParallel,
  rngMapInParallel',
  genSeeds
  )where

import GHC.Word (Word64)
import Data.Bits ((.&.))
import qualified System.Random.Mersenne.Pure64 as Mer
import Control.Monad.State.Strict
import qualified Data.Map as M
import Control.DeepSeq (force)
import Control.Parallel.Strategies


type Rng a = State Mer.PureMT a

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n l = take n l : splitList n (drop n l)

rngMapInParallel :: (NFData s) => [s] -> (s -> Rng s) -> Rng [s]
rngMapInParallel ls f = do
  g <- get
  let groups = splitList 4 ls
      (gi: gs) = genSeeds 5 g
      applyF (xs, gen) = force $ evalState (mapM f xs) gen
      ls' = concat $ parMap rpar applyF $ zip groups gs
  put gi
  return ls'

rngMapInParallel' :: (NFData s) => [(s, s)] -> ((s, s) -> Rng s) -> Rng [s]
rngMapInParallel' ls f = do
  g <- get
  let groups = splitList 4 ls
      (gi: gs) = genSeeds 5 g
      applyF (xs, gen) = force $ evalState (mapM f xs) gen
      ls' = concat $ parMap rpar applyF $ zip groups gs
  put gi
  return ls'

genSeeds :: Int -> Mer.PureMT -> [Mer.PureMT]
genSeeds n g = parMap rpar Mer.pureMT rs
  where
    rs = force $ evalState (replicateM n randomWordS) g

randomWordS :: Rng Word64
randomWordS = state Mer.randomWord64

randomProbMersenne :: Mer.PureMT -> (Double, Mer.PureMT)
randomProbMersenne g = (fromIntegral w / maxWord , g')
  where
    (w, g') = Mer.randomWord g
    maxWord = fromIntegral (maxBound :: Word) :: Double

randomBoolMersenne :: Mer.PureMT -> (Bool, Mer.PureMT)
randomBoolMersenne g = (b, g')
  where
    (w, g') = Mer.randomWord g
    b = w .&. 1 /= 0

randomRangeD :: (Double, Double) -> Mer.PureMT -> (Double, Mer.PureMT)
randomRangeD (min, max) gen = (val, g)
  where
    (r, g) = randomProbMersenne gen
    val = r * min + (1 - r) * max

randomRangeInt :: (Int, Int) -> Mer.PureMT -> (Int, Mer.PureMT)
randomRangeInt (min, max) gen = (val, g)
  where
    (r, g) = Mer.randomInt gen
    val = (r `mod` (max - min + 1)) + min

randomDouble :: Double -> Double -> Rng Double
randomDouble min max = state $ randomRangeD (min::Double, max::Double)

randomProbability :: Rng Double
randomProbability = state randomProbMersenne

randomBool :: Rng Bool
randomBool = state randomBoolMersenne

randomInt :: Int -> Int -> Rng Int
randomInt min max = state $ randomRangeInt (min::Int, max::Int)

boxMuller :: Floating a => a -> a -> (a, a)
boxMuller u1 u2 = (r * cos t, r * sin t)
  where
    r = sqrt (-2 * log u1)
    t = 2 * pi * u2

gaussian :: Double -> Double -> Mer.PureMT -> (Double, Mer.PureMT)
gaussian sd m g = (n1 * sd + m, g2)
  where
    (u1, g1) = randomProbMersenne g
    (u2, g2) = randomProbMersenne g1
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

fisherYatesStep :: (M.Map Int a, Mer.PureMT ) -> (Int, a) -> (M.Map Int a, Mer.PureMT )
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomRangeInt (0, i) gen

fisherYates :: [a] -> Mer.PureMT -> ([a], Mer.PureMT )
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
