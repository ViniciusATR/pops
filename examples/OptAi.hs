{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Pops.Solution
import Pops.Operators
import Pops.OptAi
import Pops.Rng
import System.Random
import Control.Monad.State.Strict
import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )


triggerTrim = createTrimmingOperator 0.01 100
cloneSelection = createClonalSelection 100.0 10

optai = Select triggerTrim (PopMod cloneSelection End) End

main :: IO ()
main = do
  let pops = executeAlgorithm 42 1000 20 optai
  print $ getBest pops
