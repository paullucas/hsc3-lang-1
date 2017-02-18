-- | 'getStdRandom' based @sclang@ random number functions.
module Sound.SC3.Lang.Random.IO where

import Control.Monad.IO.Class {- transformers -}
import System.Random {- random -}

import Sound.SC3.Lang.Core
import qualified Sound.SC3.Lang.Random.Gen as R

-- | 'liftIO' of 'randomRIO'.
randomM :: (Random a, MonadIO m) => (a, a) -> m a
randomM = liftIO . randomRIO

-- | 'liftIO' of 'getStdRandom'.
randomG :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
randomG = liftIO . getStdRandom

-- | 'R.rand'.
--
-- > import Control.Monad
-- > replicateM 10 (rand 100)
rand :: (MonadIO m,Random n,Num n) => n -> m n
rand = randomG . R.rand

-- | 'R.nrand'.
nrand :: (Random a, Num a) => Int -> a -> IO [a]
nrand = randomG .: R.nrand

-- | 'R.rand2'.
rand2 :: (MonadIO m,Random n,Num n) => n -> m n
rand2 = randomG . R.rand2

-- | 'R.nrand2'.
nrand2 :: (Random a, Num a) => Int -> a -> IO [a]
nrand2 = randomG .: R.nrand2

-- | 'R.rrand'.
rrand :: (MonadIO m,Random n) => n -> n -> m n
rrand = randomG .: R.rrand

-- | 'R.nrrand'.
--
-- > nrrand 9 (-9) 9
nrrand :: (MonadIO m,Random a) => Int -> a -> a -> m [a]
nrrand = randomG .:: R.nrrand

-- | 'R.choose'.
choose :: MonadIO m => [a] -> m a
choose = randomG . R.choose

-- | 'R.nchoose'.
nchoose :: MonadIO m => Int -> [a] -> m [a]
nchoose = randomG .: R.nchoose

-- | 'R.exprand'.
exprand :: (MonadIO m,Floating n,Random n) => n -> n -> m n
exprand = randomG .: R.exprand

-- | 'R.exprand'.
nexprand :: (MonadIO m,Floating n,Random n) => Int -> n -> n -> m [n]
nexprand = randomG .:: R.nexprand

-- | 'R.coin'.
coin :: (MonadIO m,Random n,Fractional n,Ord n) => n -> m Bool
coin = randomG . R.coin

-- | 'R.coin'.
ncoin :: (MonadIO m,Random n,Fractional n,Ord n) => Int -> n -> m [Bool]
ncoin = randomG .: R.ncoin

-- | 'R.scramble'.
scramble :: MonadIO m => [t] -> m [t]
scramble = randomG . R.scramble

-- | 'R.wchoose'.
wchoose :: (MonadIO m,Random a,Ord a,Fractional a) => [b] -> [a] -> m b
wchoose = randomG .: R.wchoose

-- | 'R.wchoose_N'.
wchoose_N :: (MonadIO m,Random a,Ord a,Fractional a) => [b] -> [a] -> m b
wchoose_N = randomG .: R.wchoose_N

-- | 'R.nwchoose_N'.
nwchoose_N :: (MonadIO m,Random a,Ord a,Fractional a) => Int -> [b] -> [a] -> m [b]
nwchoose_N = randomG .:: R.nwchoose_N
