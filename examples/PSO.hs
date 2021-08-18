{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Pops.Data
import Pops.Operators
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy)


data PSOSolution a = PSOSolution {
                                  value :: a,
                                  velocity :: a,
                                  bestPosition :: a,
                                  fitness :: Double
                               } deriving(Show)

instance Solution PSOSolution [Double] where
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
    pos <- replicateM n $ randomDouble (-dim) dim
    vel <- replicateM n $ randomDouble (-maxv) maxv
    let newSol = PSOSolution pos vel pos 0.0
    return $ updateSolution newSol pos
      where
        n = 2
        dim = 5.12
        maxv = 1.0


updateBestPosition :: PSOSolution [Double] -> PSOSolution [Double]
updateBestPosition sol = if currentCost > bestCost
                            then sol {bestPosition = value sol}
                            else sol
  where
    bestCost = cost $ sol { value = bestPosition sol }
    currentCost = fitness sol

updatePosition :: PSOSolution [Double] -> PSOSolution [Double]
updatePosition sol = sol { value = newValue }
  where newValue = zipWith (+) (value sol) (velocity sol)

validateVelocity :: Double -> Double
validateVelocity vel
  | vel > max = max
  | vel < -max = -max
  | otherwise = vel
    where max = 1.0

getBestPosition :: [PSOSolution [Double]] -> PSOSolution [Double]
getBestPosition = minimumBy (\a b -> compare (cost a) (cost b))

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
      let newVelocity = map validateVelocity $ zipWith3 (\a b c -> a + b + c) cv globalcoeff localcoeff
      let newSolution = updateBestPosition $ updatePosition $ sol { velocity = newVelocity }
      return newSolution

pso = PopMod changeVelocity End

main :: IO ()
main = do
  let pops = executeAlgorithm 42 10 20 pso
  print $ getBestPosition pops
