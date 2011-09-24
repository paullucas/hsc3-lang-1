module Sound.SC3.Lang.Random.Monad where

import Control.Monad
import Control.Monad.Random {- MonadRandom -}
import qualified Sound.SC3.Lang.Math as M

-- | @SimpleNumber.rand@ is 'getRandomR' in (0,/n/).
--
-- > evalRand (replicateM 2 (rand 10)) (mkStdGen 6) == [5,8]
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
rrand :: (RandomGen g,Random n,Num n) => n -> n -> Rand g n
rrand l r = getRandomR (l,r)

-- | Variant of 'rrand' generating /k/ values.
--
-- > evalRand (nrrand 4 3 9) (mkStdGen 1) == [5,8,9,6]
nrrand :: (RandomGen g,Random n,Num n) => Int -> n -> n -> Rand g [n]
nrrand k l = replicateM k . rrand l

-- | @SequenceableCollection.choose@ selects an element at random.
--
-- > evalRand (choose [3..9]) (mkStdGen 1) == 5
choose :: RandomGen g => [a] -> Rand g a
choose l = do
  i <- rand (length l - 1)
  return (l !! i)

-- | Variant of 'choose' generating /k/ values.
--
-- > evalRand (nchoose 4 [3..9]) (mkStdGen 1) == [5,8,9,6]
nchoose :: (RandomGen g) => Int -> [a] -> Rand g [a]
nchoose k = replicateM k . choose

-- | @SimpleNumber.exprand@ exponentially distributed random number in
-- the interval.
--
-- > let r = replicateM 3 (exprand 10 100 >>= return.floor)
-- > in evalRand r (mkStdGen 1) == [22,21,13]
exprand :: (Floating n,Random n,RandomGen g) => n -> n -> Rand g n
exprand l r = do
  n <- rrand 0.0 1.0
  return (M.exprandrng l r n)

-- | Variant of 'exprand' generating /k/ values.
--
-- > let r = nexprand 3 10 100 >>= return . map floor
-- > in evalRand r (mkStdGen 1) == [22,21,13]
nexprand :: (Floating n,Random n,RandomGen g) => Int -> n -> n -> Rand g [n]
nexprand k l = replicateM k . exprand l
