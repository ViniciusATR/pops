module Pops.Rng(
  Rng,
  randomDouble,
  randomProbability,
  randomBool,
  randomInt,
  randomGaussian,
  randomGaussian',
  sample
  )where

import System.Random
import Control.Monad.State.Strict

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
