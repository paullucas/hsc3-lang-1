module Sound.SC3.Lang.Math.SimpleNumber where

import System.Random {- random -}

exprand :: (Floating b) => b -> b -> b -> b
exprand l r i = l * (log (r / l) * i)

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

inf :: Bounded a => a
inf = maxBound

isInf :: (Eq a,Bounded a) => a -> Bool
isInf = (== inf)

linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp l r l' r' n =
    if n <= l
    then l'
    else if n >= r
         then r'
         else ((r'/l') ** ((n-l)/(r-l))) * l'

linexp_ :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp_ n l r l' r' = linexp l r l' r' n

{-
import Control.Monad
replicateM 12 (coin 0.5)
let g = mkStdGen 0
rrand' 0.0 1.0 g
coin' 0.5 g
-}
