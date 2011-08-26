module Sound.SC3.Lang.Random.Monad where

import Control.Monad.Random {- MonadRandom -}
import qualified Sound.SC3.Lang.Math as M

rand :: (RandomGen g,Random n,Num n) => n -> Rand g n
rand n = getRandomR (0,n)

nrand :: (RandomGen g,Random n,Num n) => Int -> n -> Rand g [n]
nrand k n = sequence (replicate k (rand n))

rand2 :: (RandomGen g,Random n,Num n) => n -> Rand g n
rand2 n = getRandomR (-n,n)

nrand2 :: (RandomGen g,Random n,Num n) => Int -> n -> Rand g [n]
nrand2 k n = sequence (replicate k (rand2 n))

rrand :: (RandomGen g,Random n,Num n) => n -> n -> Rand g n
rrand l r = getRandomR (l,r)

nrrand :: (RandomGen g,Random n,Num n) => Int -> n -> n -> Rand g [n]
nrrand k l r = sequence (replicate k (rrand l r))

choose :: RandomGen g => [a] -> Rand g a
choose l = do
  i <- rand (length l - 1)
  return (l !! i)

nchoose :: (RandomGen g) => Int -> [a] -> Rand g [a]
nchoose k l = sequence (replicate k (choose l))

exprand :: (Floating n,Random n,RandomGen g) => n -> n -> Rand g n
exprand l r = do
  n <- rrand 0.0 1.0
  return (M.exprandrng l r n)
