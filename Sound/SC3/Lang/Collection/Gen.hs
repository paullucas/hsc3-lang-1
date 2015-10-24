-- | Implementaion of server b_gen routines.
module Sound.SC3.Lang.Collection.Gen where

import Data.List

import qualified Sound.SC3.Lang.Collection as C {- hsc3-lang -}
import qualified Sound.SC3.Lang.Math.Chebyshev as M {- hsc3-lang -}

two_pi :: Floating n => n
two_pi = 2 * pi

-- * sine1

sine1_p :: (Enum n,Floating n) => Int -> (n,n) -> [n]
sine1_p n (pfreq,ampl) = sine3_p n (pfreq,ampl,0)

sine1_l :: (Enum n,Floating n) => Int -> [n] -> [[n]]
sine1_l n ampl = map (sine1_p n) (zip [1..] ampl)

sum_l :: Num n => [[n]] -> [n]
sum_l = map sum . transpose

-- > import Sound.SC3.Plot
-- > plotTable1 (sine1 256 [1,0.95 .. 0.5])
-- > plotTable1 (C.normalize 0.95 1.05 (sine1 256 [1,0.95 .. 0.5]))
sine1 :: (Enum n,Floating n) => Int -> [n] -> [n]
sine1 n = sum_l . sine1_l n

nrm_u :: (Fractional n,Ord n) => [n] -> [n]
nrm_u = C.normalize (-1) 1

-- > plotTable1 (sine1_nrm 256 [1,0.95 .. 0.5])
sine1_nrm :: (Enum n,Floating n,Ord n) => Int -> [n] -> [n]
sine1_nrm n = nrm_u . sine1 n

-- * sine2

sine2_l :: (Enum n,Floating n) => Int -> [(n,n)] -> [[n]]
sine2_l n = map (sine1_p n)

-- > plotTable1 (sine2 256 (zip [1,2..] [1,0.95 .. 0.5]))
-- > plotTable1 (sine2 256 (zip [1,1.5 ..] [1,0.95 .. 0.5]))
sine2 :: (Enum n,Floating n) => Int -> [(n,n)] -> [n]
sine2 n = sum_l . sine2_l n

sine2_nrm :: (Enum n,Floating n,Ord n) => Int -> [n] -> [n]
sine2_nrm n = nrm_u . sine1 n

-- * sine3

sine3_p :: (Enum n,Floating n) => Int -> (n,n,n) -> [n]
sine3_p n (pfreq,ampl,phase) =
    let incr = (two_pi / (fromIntegral n - 1)) * pfreq
    in map ((*) ampl . sin) (take n [phase,phase + incr ..])

sine3_l :: (Enum n,Floating n) => Int -> [(n,n,n)] -> [[n]]
sine3_l n = map (sine3_p n)

-- > plotTable1 (sine3 256 (zip3 [1,1.5 ..] [1,0.95 .. 0.5] [0,pi/7..]))
sine3 :: (Enum n,Floating n) => Int -> [(n,n,n)] -> [n]
sine3 n = sum_l . sine3_l n

-- * cheby

cheby :: (Enum n, Floating n, Ord n) => Int -> [n] -> [n]
cheby = M.gen_cheby
