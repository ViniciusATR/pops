{-# LANGUAGE FlexibleInstances #-}
module Main where

import Pops.Solution
import System.Random
import Control.Monad.State.Strict

randomDouble :: Double -> Double -> Rng Double
randomDouble min max = state $ randomR (min::Double, max::Double)

instance Representation [Double] where
  cost s =  10 * n + cumsum
    where
      n = fromIntegral $ length s
      inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) s
      cumsum = sum inner

type Vec = [Double]

data PSOSolution = PSOSolution {
                                  value :: Vec,
                                  velocity :: Vec,
                                  bestPosition :: Vec,
                                  fitness :: Double
                               } deriving(Show)

instance Solution PSOSolution where
  createRandom = do
    pos <- replicateM n $ randomDouble (-dim) dim
    vel <- replicateM n $ randomDouble (-maxv) maxv
    return $ PSOSolution pos vel pos (cost pos)
      where
        n = 2
        dim = 5.12
        maxv = 1.0

main :: IO ()
main = do
  let gen = mkStdGen 42
  let sol = evalState createRandom gen :: PSOSolution
  print sol
