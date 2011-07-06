module Sound.SC3.Lang.Collection.Numerical where

import Sound.SC3.Lang.Collection.SequenceableCollection (zipWith_c)

instance Num a => Num [a] where
    negate = map negate
    (+) = zipWith_c (+)
    (-) = zipWith_c (-)
    (*) = zipWith_c (*)
    abs = map abs
    signum = map signum
    fromInteger n = [fromInteger n]

instance Fractional a => Fractional [a] where
    recip = map recip
    (/) = zipWith_c (/)
    fromRational n = [fromRational n]

instance Floating a => Floating [a] where
    pi = cycle [pi]
    exp = map exp
    log = map log
    sqrt = map sqrt
    (**) = zipWith_c (**)
    logBase = zipWith_c logBase
    sin = map sin
    cos = map cos
    tan = map tan
    asin = map asin
    acos = map acos
    atan = map atan
    sinh = map sinh
    cosh = map cosh
    tanh = map tanh
    asinh = map asinh
    acosh = map acosh
    atanh = map atanh
