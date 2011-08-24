module Sound.SC3.Lang.Math.SimpleNumber where

import System.Random {- random -}

exprandrng :: (Floating b) => b -> b -> b -> b
exprandrng l r i = l * exp (log (r / l) * i)

exprand' :: (Floating n,Random n,RandomGen g) => n -> n -> g -> (n,g)
exprand' l r g =
    let (n,g') = rrand' 0.0 1.0 g
    in (exprandrng l r n,g')

exprand :: (Floating n,Random n) => n -> n -> IO n
exprand l r = rrand 0.0 1.0 >>= return . exprandrng l r

rrand' :: (Random n, RandomGen g) => n -> n -> g -> (n,g)
rrand' = curry randomR

nrrand' :: (RandomGen g,Random a,Num a) => Int -> a -> a -> g -> ([a],g)
nrrand' n l r =
    let go x 0 g = (x,g)
        go x k g =
            let (y,g') = randomR (l,r) g
            in go (y:x) (k - 1) g'
    in go [] n

nrrand :: (Random a, Num a) => Int -> a -> a -> IO [a]
nrrand n l = getStdRandom . nrrand' n l

rrand :: (Random n) => n -> n -> IO n
rrand = curry randomRIO

rand' :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand' n = randomR (0,n)

rand :: (Random n,Num n) => n -> IO n
rand n = randomRIO (0,n)

rand2' :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand2' n = randomR (-n,n)

rand2 :: (Random n,Num n) => n -> IO n
rand2 n = randomRIO (-n,n)

nrand2' :: (RandomGen g,Random a,Num a) => Int -> a -> g -> ([a],g)
nrand2' n i = nrrand' n 0 i

nrand2 :: (Random a, Num a) => Int -> a -> IO [a]
nrand2 n = getStdRandom . nrand2' n

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
