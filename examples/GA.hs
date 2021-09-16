import Pops.Solution
import Pops.GA
import Pops.Populational
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy, tails, sortBy)
import System.Environment


data GASolution = GASolution {
    value :: [Double],
    fitness :: Double
} deriving(Show)

instance Solution GASolution where
  cost sol =  10 * n + cumsum
    where
      s = value sol
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

  createRandom = do
    let newSol = GASolution [0.0, 0.0] 0.0
    vec <- replicateM 2 $ randomDouble (-5.12) 5.12
    return $ updateSolution newSol vec

instance SimpleSolution GASolution where
  getValue s = value s

  updateSolution current newValue = intermediate { fitness = fitness' }
    where
      intermediate = current { value = newValue }
      fitness' = cost intermediate


main :: IO ()
main = do
  args <- getArgs
  let seed = read $ args!!0 :: Int
      maxIterations = read $ args!!1 :: Int
      popSize = read $ args!!2 :: Int

      mutate :: IndividualModifier GASolution
      mutate = createMutationOperator 0.5

      crossover :: PopulationalModifier GASolution
      crossover = createCrossoverOperator 0.5 popSize

      truncateSelect :: PopulationalModifier GASolution
      truncateSelect = createTruncationSelection 10 popSize

      ga :: Populational GASolution
      ga = PopMod truncateSelect $ PopMod crossover $ IndMod mutate End

      pops = executeAlgorithm seed popSize maxIterations ga
  print $ getBest pops
