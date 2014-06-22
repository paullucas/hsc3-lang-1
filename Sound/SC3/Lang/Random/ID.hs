-- | 'ID' variants of "Sound.SC3.Lang.Random.Gen".
module Sound.SC3.Lang.Random.ID where

import System.Random {- random -}

import qualified Sound.SC3.Lang.Random.Gen as G

id_rand :: Enum e => e -> (StdGen -> (a,StdGen)) -> a
id_rand e f = fst (f (mkStdGen (fromEnum e)))

nchoose :: Enum e => e -> Int -> [a] -> [a]
nchoose e n l = id_rand e (G.nchoose n l)

rand :: (Random a, Num a, Enum e) => e -> a -> a
rand e n = id_rand e (G.rand n)

rrand :: (Random a, Num a, Enum e) => e -> a -> a -> a
rrand e l r = id_rand e (G.rrand l r)
