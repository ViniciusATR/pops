{-# LANGUAGE FlexibleInstances #-}

import Pops.Data
import Pops.Operators
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy)


instance Representation [Double] where
  cost s =  10 * n + cumsum
    where
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

data PSOSolution a = PSOSolution {
                                  value :: a,
                                  velocity :: a,
                                  bestPosition :: a,
                                  fitness :: Double
                               } deriving(Show)

instance Solution (PSOSolution [Double]) where
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


getBestPosition :: [PSOSolution [Double]] -> PSOSolution [Double]
getBestPosition = minimumBy (\a b -> compare (cost $ value a) (cost $ value b))

changeVelocity :: PopulationalModifier (PSOSolution [Double])
changeVelocity pop = mapM (mod' gbest) pop
  where
    gbest = value $ getBestPosition pop
    mod' :: [Double] -> PSOSolution [Double] -> Rng (PSOSolution [Double])
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

pso = PopMod changeVelocity End

main :: IO ()
main = do
  let pops = executeAlgorithm 42 10 20 pso
  print $ getBestPosition pops
