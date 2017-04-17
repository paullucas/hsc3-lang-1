-- | 'Rand' monad based @sclang@ random number functions.
module Sound.SC3.Lang.Random.Monad where

import Control.Monad {- base -}
import Control.Monad.Random {- MonadRandom -}
import Data.Maybe {- base  -}

import Sound.SC3.Common.Buffer {- hsc3 -}

import qualified Sound.SC3.Lang.Collection as C
import Sound.SC3.Lang.Core ((.:))
import qualified Sound.SC3.Lang.Math as M

-- | @SimpleNumber.rand@ is 'getRandomR' in (0,/n/).
--
-- > evalRand (replicateM 2 (rand (10::Int))) (mkStdGen 6) == [5,8]
-- > evalRand (rand (1::Double)) (mkStdGen 6) == 0.21915126172825694
rand :: (RandomGen g,Random n,Num n) => n -> Rand g n
rand n = getRandomR (0,n)

-- | Variant of 'rand' generating /k/ values.
--
-- > evalRand (nrand 3 10) (mkStdGen 6) == [5,8,1]
nrand :: (RandomGen g,Random n,Num n) => Int -> n -> Rand g [n]
nrand k = replicateM k . rand

-- | @SimpleNumber.rand2@ is 'getRandomR' in (-/n/,/n/).
--
-- > evalRand (replicateM 2 (rand2 10)) (mkStdGen 5) == [7,-6]
rand2 :: (RandomGen g,Random n,Num n) => n -> Rand g n
rand2 n = getRandomR (-n,n)

-- | Variant of 'rand2' generating /k/ values.
--
-- > evalRand (nrand2 3 10) (mkStdGen 5) == [7,-6,9]
nrand2 :: (RandomGen g,Random n,Num n) => Int -> n -> Rand g [n]
nrand2 k = replicateM k . rand2

-- | @SimpleNumber.rrand@ is 'curry' 'getRandomR'.
--
-- > evalRand (replicateM 2 (rrand 3 9)) (mkStdGen 1) == [5,8]
rrand :: (RandomGen g,Random n) => n -> n -> Rand g n
rrand l r = getRandomR (l,r)

-- | Variant of 'rrand' generating /k/ values.
--
-- > evalRand (nrrand 4 3 9) (mkStdGen 1) == [5,8,9,6]
nrrand :: (RandomGen g,Random n) => Int -> n -> n -> Rand g [n]
nrrand k = replicateM k .: rrand

-- | @SequenceableCollection.choose@ selects an element at random.
--
-- > evalRand (choose [3..9]) (mkStdGen 1) == 5
choose :: RandomGen g => [a] -> Rand g a
choose l = do
  i <- rand (length l - 1)
  return (l !! i)

wchoose :: (RandomGen g,Fractional t,Ord t,Random t) => [a] -> [t] -> Rand g a
wchoose l w = do
  i <- rrand 0.0 1.0
  let n = fromMaybe (error "wchoose: windex") (C.windex w i)
  return (l !! n)

wchoose_N :: (RandomGen g,Fractional t,Ord t,Random t) => [a] -> [t] -> Rand g a
wchoose_N l w = wchoose l (normalizeSum w)

-- | Variant of 'choose' generating /k/ values.
--
-- > evalRand (nchoose 4 [3..9]) (mkStdGen 1) == [5,8,9,6]
nchoose :: (RandomGen g) => Int -> [a] -> Rand g [a]
nchoose k = replicateM k . choose

-- | @SimpleNumber.exprand@ generates exponentially distributed random
-- number in the given interval.
--
-- > let r = replicateM 3 (exprand 10 100 >>= return.floor)
-- > in evalRand r (mkStdGen 1) == [22,21,13]
exprand :: (Floating n,Random n,RandomGen g) => n -> n -> Rand g n
exprand l r = do
  n <- rrand 0.0 1.0
  return (M.exprange l r n)

-- | Variant of 'exprand' generating /k/ values.
--
-- > let r = nexprand 3 10 100 >>= return . map floor
-- > in evalRand r (mkStdGen 1) == [22,21,13]
nexprand :: (Floating n,Random n,RandomGen g) => Int -> n -> n -> Rand g [n]
nexprand k = replicateM k .: exprand

