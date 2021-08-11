{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Pops.Data
import Pops.Operators
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )


data AISolution a = AISolution {
                                  value :: a,
                                  fitness :: Double,
                                  normalizedFitness :: Double
                               } deriving(Show)


instance Solution AISolution [Double] where
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
    let newSol = AISolution [0.0, 0.0] 0.0 0.0
    vec <- replicateM 2 $ randomDouble (-5.12) 5.12
    return $ updateSolution newSol vec

distance :: AISolution [Double] -> AISolution [Double] -> Double
distance a b = sqrt $ sum sumOfSquares
  where
    sumOfSquares = zipWith (\x y -> (x - y)**2) (value a) (value b)


avgCost :: [AISolution [Double]] -> Double
avgCost pop =  sum (map cost pop) / n
  where
    n = fromIntegral $ length pop

getBest :: [AISolution [Double]] -> AISolution [Double]
getBest = minimumBy (\a b -> compare (cost a) (cost b))

getWorst :: [AISolution [Double]] -> AISolution [Double]
getWorst = maximumBy (\a b -> compare (cost a) (cost b))

normalizeFitness :: [AISolution [Double]] -> [AISolution [Double]]
normalizeFitness pop = map normalize pop
  where
    min = cost $ getBest pop
    max = cost $ getWorst pop
    range = max - min
    normalize sol = AISolution (value sol) (fitness sol) normalized
      where
        normalized = fitness sol - min / range

mutate :: IndividualModifier (AISolution [Double])
mutate sol = do
  modVec <- replicateM 2 randomGaussian'
  let alpha = (1.0/100.0) * exp ( - (normalizedFitness sol) )
  let newVec = zipWith (\s r -> s + r * alpha) (value sol) modVec
  return $ updateSolution sol newVec

cloneSelection :: PopulationalModifier (AISolution [Double])
cloneSelection pop = do
  let pop' = normalizeFitness pop
  mapM individualClone pop'
  where
    individualClone sol = do
      mutatedClones <- replicateM 10 (mutate sol)
      return $ getBest $ mutatedClones ++ [sol]

trim :: PopulationalModifier (AISolution [Double])
trim pop = do
  let neighbours = groupBy (\a b -> distance a b <= 0.01) pop
  trimmed <- mapM sample neighbours
  newCells <- replicateM 100 createRandom
  return $ trimmed ++ newCells

triggerTrim :: Selector (AISolution [Double])
triggerTrim old new = do
  let currAvg = avgCost old
  let newAvg = avgCost new
  if newAvg < currAvg then do
    return new
  else do
    trim new

optai = Select triggerTrim (PopMod cloneSelection End) End

main :: IO ()
main = do
  let pops = executeAlgorithm 42 1000 20 optai
  print $ getBest pops