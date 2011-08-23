{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sound.SC3.Lang.Collection.Numerical.Truncating where

instance (Num a) => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = map abs
    signum = map signum
    fromInteger = repeat . fromInteger
    negate = map negate

instance (Fractional a) => Fractional [a] where
    (/) = zipWith (/)
    recip = map recip
    fromRational = repeat . fromRational

instance Floating a => Floating [a] where
    pi = repeat pi
    exp = map exp
    log = map log
    sqrt = map sqrt
    (**) = zipWith (**)
    logBase = zipWith logBase
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

{-
[1,2,3] * [4,5]
[1,2,3] * 2
-}
