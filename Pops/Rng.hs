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
  randomWeightedChoice
  )where

import System.Random
import Control.Monad.State.Strict
import qualified Data.Map as M

type Rng a = State StdGen a

randomDouble :: Double -> Double -> Rng Double
randomDouble min max = state $ randomR (min::Double, max::Double)

randomProbability :: Rng Double
randomProbability = state $ randomR (0.0::Double, 1.0::Double)

randomBool :: Rng Bool
randomBool = state $ randomR (True, False)

randomInt :: Int -> Int -> Rng Int
randomInt min max = state $ randomR (min::Int, max::Int)

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

sample :: [a] -> Rng a
sample xs = do
  idx <- randomInt 0 (n - 1)
  return $ xs !! idx
    where n = length xs

-- Implementação de um shuffle de listas de acordo com
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
-- Obtido também em https://wiki.haskell.org/Random_shuffle

fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => [a] -> g -> ([a], g)
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
