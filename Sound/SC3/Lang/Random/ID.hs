-- | 'ID' variants of "Sound.SC3.Lang.Random.Gen".
module Sound.SC3.Lang.Random.ID where

import System.Random {- random -}

import qualified Sound.SC3.Lang.Random.Gen as G

id_rand :: Enum e => e -> (StdGen -> (a,StdGen)) -> a
id_rand e f = fst (f (mkStdGen (fromEnum e)))

nchoose :: Enum e => e -> Int -> [a] -> [a]
nchoose e n l = id_rand e (G.nchoose n l)
