module Sound.SC3.Lang.Math.SimpleNumber where

import System.Random {- random -}

rrand' :: (Random n, RandomGen g) => n -> n -> g -> (n,g)
rrand' = curry randomR

rrand :: (Random n) => n -> n -> IO n
rrand = curry randomRIO

rand :: (Random n,Num n) => n -> IO n
rand n = randomRIO (0,n)

rand2 :: (Random n,Num n) => n -> IO n
rand2 n = randomRIO (-n,n)

coin' :: (RandomGen g, Random a, Ord a, Fractional a) => a -> g -> (Bool,g)
coin' n g =
  let (i,g') = randomR (0.0,1.0) g
  in (i < n,g')

coin :: (Random n,Fractional n,Ord n) => n -> IO Bool
coin = getStdRandom . coin'

{-
import Control.Monad
replicateM 12 (coin 0.5)
let g = mkStdGen 0
rrand' 0.0 1.0 g
coin' 0.5 g
-}
