{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | List instances of the standard haskell numerical classes with SC3
-- extension behaviour.  Provides instances for 'Num', 'Fractional'
-- and 'Floating'.
--
-- > [1,2] + [3,4,5] == [4,6,6]
-- > [1,2,3] * [4,5] == [4,10,12]
--
-- Literals are interpreted as single element lists.
--
-- > [1,2,3] + 4 == [5,6,7]
-- > [1,2,3] * 4 == [4,8,12]
module Sound.SC3.Lang.Collection.Numerical.Extending where

import Sound.SC3.Lang.Collection.Extension (zipWith_c)

instance Num a => Num [a] where
    negate = map negate
    (+) = zipWith_c (+)
    (-) = zipWith_c (-)
    (*) = zipWith_c (*)
    abs = map abs
    signum = map signum
    fromInteger n = [fromInteger n]

instance Real a => Real [a] where
  toRational = error "[Real], toRational"

instance Enum a => Enum [a] where
  succ = map succ
  pred = map pred
  toEnum = return . toEnum
  fromEnum = error "[Enum], fromEnum"
  enumFrom = error "[Enum]"
  enumFromThen = error "[Enum]"
  enumFromTo = error "[Enum]"
  enumFromThenTo = error "[Enum]"

instance Integral a => Integral [a] where
  quot = zipWith_c quot
  rem = zipWith_c rem
  div = zipWith_c div
  mod = zipWith_c mod
  quotRem = error "[Integral] is partial"
  divMod = error "[Integral] is partial"
  toInteger = error "[Integral] is partial"

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
