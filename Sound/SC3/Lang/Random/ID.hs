-- | 'ID' variants of "Sound.SC3.Lang.Random.Gen".
module Sound.SC3.Lang.Random.ID where

import System.Random {- random -}

import qualified Sound.SC3.Lang.Random.Gen as G

id_rand :: Enum e => e -> (StdGen -> (a,StdGen)) -> a
id_rand e f = fst (f (mkStdGen (fromEnum e)))

-- | /n/ iterations of 'choose'.
--
-- > nchoose 'α' 9 [3,5] == [3,3,5,5,3,3,5,3,5]
nchoose :: Enum e => e -> Int -> [a] -> [a]
nchoose e n l = id_rand e (G.nchoose n l)

rand :: (Random a, Num a, Enum e) => e -> a -> a
rand e n = id_rand e (G.rand n)

rrand :: (Random a, Num a, Enum e) => e -> a -> a -> a
rrand e l r = id_rand e (G.rrand l r)

-- | /n/ iterations of 'rrand'.
--
-- > nrrand 'α' 9 1 9 == [8,3,4,7,4,9,2,9,3]
nrrand :: (Random a, Num a, Enum e) => e -> Int -> a -> a -> [a]
nrrand e n l r = id_rand e (G.nrrand n l r)

-- | /n/ iterations of 'coin'.
--
-- > ncoin 'α' 9 0.5 == [True,True,True,False,False,False,False,True,False]
ncoin :: (Random a, Ord a, Fractional a, Enum e) => e -> Int -> a -> [Bool]
ncoin e n w = id_rand e (G.ncoin n w)

scramble :: Enum e => e -> [t] -> [t]
scramble e = id_rand e . G.scramble

