-- | 'getStdRandom' based @sclang@ random number functions.
module Sound.SC3.Lang.Random.IO where

import Control.Monad.IO.Class
import Sound.SC3.Lang.Random.Gen as R
import System.Random {- random -}

randomM :: (Random a, MonadIO m) => (a, a) -> m a
randomM = liftIO . randomRIO

-- | @SimpleNumber.rand@ is 'randomRIO' in (0,/n/).
rand :: (MonadIO m,Random n,Num n) => n -> m n
rand n = randomM (0,n)

-- | @SimpleNumber.rand2@ is 'randomRIO' in (-/n/,/n/).
rand2 :: (MonadIO m,Random n,Num n) => n -> m n
rand2 n = randomM (-n,n)

randomG :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
randomG = liftIO . getStdRandom

-- | Variant of 'rand2' generating /k/ values.
nrand2 :: (Random a, Num a) => Int -> a -> IO [a]
nrand2 k = randomG . R.nrand2 k

-- | @SimpleNumber.rrand@ is 'curry' 'randomRIO'.
rrand :: (MonadIO m,Random n) => n -> n -> m n
rrand l r = randomM (l,r)

-- | Variant of 'rrand' generating /k/ values.
nrrand :: (MonadIO m,Random a, Num a) => Int -> a -> a -> m [a]
nrrand k l = randomG . R.nrrand k l

-- | @SequenceableCollection.choose@ selects an element at random.
choose :: MonadIO m => [a] -> m a
choose = randomG . R.choose

-- | @SimpleNumber.exprand@ generates exponentially distributed random
-- number in the given interval.
exprand :: (MonadIO m,Floating n,Random n) => n -> n -> m n
exprand l = randomG . R.exprand l

-- | @SimpleNumber.coin@ is 'True' at given probability, which is in
-- range (0,1).
coin :: (MonadIO m,Random n,Fractional n,Ord n) => n -> m Bool
coin = randomG . R.coin

-- | @List.scramble@ shuffles the elements.
scramble :: MonadIO m => [t] -> m [t]
scramble = randomG . R.scramble

-- | @SequenceableCollection.wchoose@ selects an element from a list
-- given a list of weights which sum to @1@.
wchoose :: (MonadIO m,Random a,Ord a,Fractional a) => [b] -> [a] -> m b
wchoose l = randomG . R.wchoose l

