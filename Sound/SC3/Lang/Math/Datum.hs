module Sound.SC3.Lang.Math.Datum where

import GHC.Exts (IsString(..))
import Sound.OpenSoundControl.Type (Datum(..))

instance IsString Datum where
    fromString = String

datum_lift :: (Int -> Int) -> (Double -> Double) -> (Datum -> Datum)
datum_lift fi fd d =
    case d of
      Int n -> Int (fi n)
      Float n -> Float (fd n)
      Double n -> Double (fd n)
      _ -> error "datum_lift"

datum_lift' :: (Double -> Double) -> (Datum -> Datum)
datum_lift' f = datum_lift (error "datum_lift:non integral") f

type I_Binop = Int -> Int -> Int
type F_Binop = Double -> Double -> Double

datum_lift2 :: I_Binop -> F_Binop -> (Datum -> Datum -> Datum)
datum_lift2 fi fd d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> Int (fi n1 n2)
      (Float n1,Float n2) -> Float (fd n1 n2)
      (Double n1,Double n2) -> Double (fd n1 n2)
      _ -> error "datum_lift2"

datum_lift2' :: F_Binop -> (Datum -> Datum -> Datum)
datum_lift2' f = datum_lift2 (error "datum_lift2:non integral") f

instance Num Datum where
    negate = datum_lift negate negate
    (+) = datum_lift2 (+) (+)
    (-) = datum_lift2 (-) (-)
    (*) = datum_lift2 (*) (*)
    abs = datum_lift abs abs
    signum = datum_lift signum signum
    fromInteger n = Int (fromInteger n)

instance Fractional Datum where
    recip = datum_lift' recip
    (/) = datum_lift2' (/)
    fromRational n = Double (fromRational n)

instance Floating Datum where
    pi = Double pi
    exp = datum_lift' exp
    log = datum_lift' log
    sqrt = datum_lift' sqrt
    (**) = datum_lift2' (**)
    logBase = datum_lift2' logBase
    sin = datum_lift' sin
    cos = datum_lift' cos
    tan = datum_lift' tan
    asin = datum_lift' asin
    acos = datum_lift' acos
    atan = datum_lift' atan
    sinh = datum_lift' sinh
    cosh = datum_lift' cosh
    tanh = datum_lift' tanh
    asinh = datum_lift' asinh
    acosh = datum_lift' acosh
    atanh = datum_lift' atanh

{-
5 :: Datum
(5 + 4) :: Datum
(2.0 ** 3.0) :: Datum
(negate 5) :: Datum
(negate 5.0) :: Datum
:set -XOverloadedStrings
"string" :: Datum
-}
