module Sound.SC3.Lang.Random.IO where

import Sound.SC3.Lang.Random.Gen as R
import System.Random {- random -}

exprand :: (Floating n,Random n) => n -> n -> IO n
exprand l = getStdRandom . R.exprand l

nrrand :: (Random a, Num a) => Int -> a -> a -> IO [a]
nrrand n l = getStdRandom . R.nrrand n l

rrand :: (Random n) => n -> n -> IO n
rrand = curry randomRIO

rand :: (Random n,Num n) => n -> IO n
rand n = randomRIO (0,n)

rand2 :: (Random n,Num n) => n -> IO n
rand2 n = randomRIO (-n,n)

nrand2 :: (Random a, Num a) => Int -> a -> IO [a]
nrand2 n = getStdRandom . R.nrand2 n

coin :: (Random n,Fractional n,Ord n) => n -> IO Bool
coin = getStdRandom . R.coin

scramble :: [t] -> IO [t]
scramble = getStdRandom . R.scramble

wchoose :: (Random a,Ord a,Fractional a) => [b] -> [a] -> IO b
wchoose l = getStdRandom . R.wchoose l

choose :: [a] -> IO a
choose = getStdRandom . R.choose
