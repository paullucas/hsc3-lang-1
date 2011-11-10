-- | 'getStdRandom' based @sclang@ random number functions.
module Sound.SC3.Lang.Random.IO where

import Sound.SC3.Lang.Random.Gen as R
import System.Random {- random -}

-- | @SimpleNumber.rand@ is 'randomRIO' in (0,/n/).
rand :: (Random n,Num n) => n -> IO n
rand n = randomRIO (0,n)

-- | @SimpleNumber.rand2@ is 'randomRIO' in (-/n/,/n/).
rand2 :: (Random n,Num n) => n -> IO n
rand2 n = randomRIO (-n,n)

-- | Variant of 'rand2' generating /k/ values.
nrand2 :: (Random a, Num a) => Int -> a -> IO [a]
nrand2 n = getStdRandom . R.nrand2 n

-- | @SimpleNumber.rrand@ is 'curry' 'randomRIO'.
rrand :: (Random n) => n -> n -> IO n
rrand = curry randomRIO

-- | Variant of 'rrand' generating /k/ values.
nrrand :: (Random a, Num a) => Int -> a -> a -> IO [a]
nrrand n l = getStdRandom . R.nrrand n l

-- | @SequenceableCollection.choose@ selects an element at random.
choose :: [a] -> IO a
choose = getStdRandom . R.choose

-- | @SimpleNumber.exprand@ generates exponentially distributed random
-- number in the given interval.
exprand :: (Floating n,Random n) => n -> n -> IO n
exprand l = getStdRandom . R.exprand l

-- | @SimpleNumber.coin@ is 'True' at given probability, which is in
-- range (0,1).
coin :: (Random n,Fractional n,Ord n) => n -> IO Bool
coin = getStdRandom . R.coin

-- | @List.scramble@ shuffles the elements.
scramble :: [t] -> IO [t]
scramble = getStdRandom . R.scramble

-- | @SequenceableCollection.wchoose@ selects an element from a list
-- given a list of weights which sum to @1@.
wchoose :: (Random a,Ord a,Fractional a) => [b] -> [a] -> IO b
wchoose l = getStdRandom . R.wchoose l

