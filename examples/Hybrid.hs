import Pops.Solution
import Pops.Populational
import qualified Pops.OptAi as OA
import qualified Pops.GA as GA
import Pops.Rng
import Control.Monad.State.Strict
import System.Environment

data HybridSolution = HybridSolution {
    value :: [Double],
    fitness :: Double,
    normalizedFitness :: Double
} deriving (Show)

instance Solution HybridSolution where
  cost sol =  10 * n + cumsum
    where
      s = value sol
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

  createRandom = do
    let newSol = HybridSolution [0.0, 0.0] 0.0 0.0
    vec <- replicateM 2 $ randomDouble (-5.12) 5.12
    return $ updateSolution newSol vec

instance SimpleSolution HybridSolution where
  getValue s = value s
  updateSolution current newValue = intermediate { fitness = fitness' }
    where
      intermediate = current { value = newValue }
      fitness' = cost intermediate

instance OA.NormalizedSolution HybridSolution where
  getNormalizedFitness s = normalizedFitness s
  updateNormalizedFitness pop = map normalize pop
    where
      min = cost $ getBest pop
      max = cost $ getWorst pop
      range = max - min
      normalize sol = HybridSolution (value sol) (fitness sol) normalized
        where
          normalized = fitness sol - min / range


main :: IO ()
main = do
  args <- getArgs
  let seed = read $ head args :: Int
      maxIterations = read $ args!!1 :: Int
      popSize = read $ args!!2 :: Int
      mutate :: IndividualModifier HybridSolution
      mutate = GA.createMutationOperator 0.5

      crossover :: PopulationalModifier HybridSolution
      crossover = GA.createCrossoverOperator 0.5 popSize

      truncateSelect :: PopulationalModifier HybridSolution
      truncateSelect = GA.createTruncationSelection 10 popSize

      cloneSelection :: PopulationalModifier HybridSolution
      cloneSelection = OA.createClonalSelection 100.0 2 10

      hybrid :: Populational HybridSolution
      hybrid = PopMod truncateSelect $ PopMod crossover $ IndMod mutate $ PopMod cloneSelection End

      pops = executeAlgorithm seed popSize maxIterations hybrid
  print $ fitness $ getBest pops
