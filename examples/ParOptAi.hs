{-# language DeriveGeneric, DeriveAnyClass, StrictData #-}
import Pops.Solution
import Pops.Populational
import Pops.OptAi
import Pops.Rng
import Control.Monad.State.Strict
import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )
import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)
import System.Environment

data AISolution = AISolution {
   value :: [Double],
   fitness :: Double,
   normalizedFitness :: Double
} deriving(Show, Generic, NFData)

instance Solution AISolution where
  cost sol =  10 * n + cumsum
    where
      s = value sol
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

  createRandom = do
    let newSol = AISolution [0.0, 0.0] 0.0 0.0
    vec <- replicateM 2 $ randomDouble (-5.12) 5.12
    return $ updateSolution newSol vec

instance SimpleSolution AISolution where
  getValue s = value s
  updateSolution current newValue = intermediate { fitness = fitness' }
    where
      intermediate = current { value = newValue }
      fitness' = cost intermediate

instance NormalizedSolution AISolution where
  getNormalizedFitness s = normalizedFitness s
  updateNormalizedFitness pop = map normalize pop
    where
      min = cost $ getBest pop
      max = cost $ getWorst pop
      range = max - min
      normalize sol = AISolution (value sol) (fitness sol) normalized
        where
          normalized = fitness sol - min / range


main :: IO ()
main = do
  args <- getArgs
  let seed = read $ head args :: Int
      maxIterations = read $ args!!1 :: Int
      popSize = read $ args!!2 :: Int

      triggerTrim :: Selector AISolution
      triggerTrim = createTrimmingOperator 0.01 100

      cloneSelection :: PopulationalModifier AISolution
      cloneSelection = createParClonalSelection 100.0 10

      optai :: Populational AISolution
      optai = Select triggerTrim (PopMod cloneSelection End) End

      pops = executeAlgorithm seed popSize maxIterations optai
  print $ fitness $ getBest pops
