import Pops.PSO
import Pops.Solution
import Pops.Populational
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy)

data PSOSolution = PSOSolution {
                                  value :: [Double],
                                  velocity :: [Double],
                                  bestPosition :: [Double],
                                  fitness :: Double
                               } deriving(Show)

instance Solution PSOSolution where
  cost sol =  10 * n + cumsum
    where
      s = value sol
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

  createRandom = do
    pos <- replicateM n $ randomDouble (-dim) dim
    vel <- replicateM n $ randomDouble (-maxv) maxv
    let newSol = PSOSolution pos vel pos 0.0
    return $ updateSolution newSol pos
      where
        n = 2
        dim = 5.12
        maxv = 1.0

instance SimpleSolution PSOSolution where
  getValue s = value s
  updateSolution current newValue = intermediate { fitness = fitness' }
    where
      intermediate = current { value = newValue }
      fitness' = cost intermediate

instance SolutionWithHistory PSOSolution where
  getBestPosition s = bestPosition s
  updateBest s new = s {bestPosition = new}

instance SolutionWithVelocity PSOSolution where
  getVelocity s = velocity s
  updateVelocity s new = s {velocity = new}

changeVelocity :: PopulationalModifier PSOSolution
changeVelocity = createChangeVelocityOperator 1.0 1.0 1.0

pso = PopMod changeVelocity (IndMod updatePosition (IndMod updateBestPosition End))

main :: IO ()
main = do
  let pops = executeAlgorithm 42 10000 1000 pso
  print $ getBest pops
