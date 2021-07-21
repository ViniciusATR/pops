{-# LANGUAGE FlexibleInstances #-}
module Main where

import Pops.Data
import Pops.Operators
import System.Random
import Control.Monad.State.Strict
import Data.List (minimumBy)


randomDouble :: Double -> Double -> Rng Double
randomDouble min max = state $ randomR (min::Double, max::Double)

randomProbability :: Rng Double
randomProbability = state $ randomR (0.0::Double, 1.0::Double)

instance Representation [Double] where
  cost s =  10 * n + cumsum
    where
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

type Vec = [Double]

data PSOSolution a = PSOSolution {
                                  value :: a,
                                  velocity :: a,
                                  bestPosition :: a,
                                  fitness :: Double
                               } deriving(Show)

instance Solution (PSOSolution Vec) where
  createRandom = do
    pos <- replicateM n $ randomDouble (-dim) dim
    vel <- replicateM n $ randomDouble (-maxv) maxv
    return $ PSOSolution pos vel pos (cost pos)
      where
        n = 2
        dim = 5.12
        maxv = 1.0


validateRoC :: Double -> Double
validateRoC roc
  | roc > max = max
  | roc < -max = -max
  | otherwise = roc
    where max = 1.0


getBestPosition :: [PSOSolution Vec] -> PSOSolution Vec
getBestPosition = minimumBy (\a b -> compare (cost $ value a) (cost $ value b))

modifySolution :: PopulationalModifier (PSOSolution Vec)
modifySolution pop = mapM (mod' gbest) pop
  where
    gbest = value $ getBestPosition pop
    mod' :: Vec -> PSOSolution Vec -> Rng (PSOSolution Vec)
    mod' gb sol = do
      let c = value sol
      let cv = velocity sol
      let lb = bestPosition sol
      r1 <- randomProbability
      r2 <- randomProbability
      let globalcoeff = zipWith (\x y -> 1.0 * r1 * (y - x)) c gb
      let localcoeff = zipWith (\x y -> 1.0 * r2 * (y - x)) c lb
      let nv = map validateRoC $ zipWith3 (\a b c -> a + b + c) cv globalcoeff localcoeff
      let np = zipWith (+) c nv
      let nb = if cost np > cost lb then np else lb
      return $ PSOSolution np nv nb (cost np)

pso = PopMod modifySolution End

main :: IO ()
main = do
  let pops = executeAlgorithm 42 10 20 pso
  print $ getBestPosition pops
