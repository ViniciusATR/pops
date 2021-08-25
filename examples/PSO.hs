{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Pops.PSO
import Pops.Data
import Pops.Operators
import Pops.Rng
import Control.Monad.State.Strict
import Data.List (minimumBy)

changeVelocity = createChangeVelocityOperator 1.0 1.0 1.0

pso = PopMod changeVelocity (IndMod updatePosition (IndMod updateBestPosition End))

main :: IO ()
main = do
  let pops = executeAlgorithm 42 10 20 pso
  print $ getBestPosition pops
