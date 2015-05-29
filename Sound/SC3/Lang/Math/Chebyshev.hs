module Sound.SC3.Lang.Math.Chebyshev where

import Data.List {- base -}

{- | Generate Chebyshev waveshaping table, see b_gen_cheby.

> import Sound.SC3.Plot
> plotTable1 (gen_cheby 256 [1,0,1,1,0,1])

-}
gen_cheby :: (Enum n, Floating n, Ord n, Integral i) => i -> [n] -> [n]
gen_cheby n =
    let acos' x = if x > 1 then 0 else if x < -1 then pi else acos x
        c k x = cos (k * acos' x)
        ix = [-1,-1 + (2 / (fromIntegral n - 1)) .. 1]
        mix = map sum . transpose
        normalize x = let m = maximum (map abs x) in map (* (recip m)) x
    in normalize . mix . map (\(k,a) -> map ((* a) . (c k)) ix) . zip [1..]
