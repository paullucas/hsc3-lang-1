{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Functions to allow using the "Sound.OpenSoundControl" 'Datum' as
-- a /universal/ data type.  In addition to the functions defined
-- below it provides instances for 'IsString', 'Num', 'Fractional',
-- 'Floating', 'Real', 'RealFrac', 'Ord', 'Enum' and 'Random'.
module Sound.SC3.Lang.Collection.Universal.Datum where

import Data.Ratio
import GHC.Exts (IsString(..))
import Sound.OpenSoundControl.Type
import System.Random

instance IsString Datum where
    fromString = String

-- | Lift an equivalent set of 'Int' and 'Double' unary functions to
-- 'Datum'.
--
-- > map (datum_lift negate negate) [Int 5,Float 5] == [Int (-5),Float (-5)]
datum_lift :: (Int -> Int) -> (Double -> Double) -> Datum -> Datum
datum_lift fi fd d =
    case d of
      Int n -> Int (fi n)
      Float n -> Float (fd n)
      Double n -> Double (fd n)
      _ -> error "datum_lift"

-- | Promote 'Int' and 'Float' 'Datum' to 'Double' 'Datum'.
--
-- > map datum_promote [Int 5,Float 5] == [Double 5,Double 5]
datum_promote :: Datum -> Datum
datum_promote d =
    case d of
      Int n -> Double (fromIntegral n)
      Float n -> Double n
      _ -> d

-- | Lift a 'Double' unary operator to 'Datum' via 'datum_promote'.
--
-- > datum_lift' negate (Int 5) == Double (-5)
datum_lift' :: (Double -> Double) -> Datum -> Datum
datum_lift' f = datum_lift (error "datum_lift:non integral") f .
                datum_promote

-- | An 'Int' binary operator.
type I_Binop = Int -> Int -> Int

-- | A 'Double' binary operator.
type F_Binop = Double -> Double -> Double

-- | Given 'Int' and 'Double' binary operators generate 'Datum'
-- operator.  If 'Datum' are of equal type result type is equal, else
-- result type is 'Double'.
--
-- > datum_lift2 (+) (+) (Float 1) (Float 2) == Float 3
-- > datum_lift2 (*) (*) (Int 3) (Float 4) == Double 12
datum_lift2 :: I_Binop -> F_Binop -> Datum -> Datum -> Datum
datum_lift2 fi fd d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> Int (fi n1 n2)
      (Float n1,Float n2) -> Float (fd n1 n2)
      (Double n1,Double n2) -> Double (fd n1 n2)
      _ -> case (datum_real d1,datum_real d2) of
             (Just n1,Just n2) -> Double (fd n1 n2)
             _ -> error "datum_lift2"

-- | A 'datum_promote' variant of 'datum_lift2'.
--
-- > datum_lift2' (+) (Float 1) (Float 2) == Double 3
datum_lift2' :: F_Binop -> Datum -> Datum -> Datum
datum_lift2' f d1 =
    let d1' = datum_promote d1
    in datum_lift2 (error "datum_lift2:non integral") f d1' .
       datum_promote

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

instance Real Datum where
    toRational d =
        case d of
          Int n -> fromIntegral n % 1
          Float n -> toRational n
          Double n -> toRational n
          _ -> error "datum,real,partial"

instance RealFrac Datum where
  properFraction d =
      let (i,j) = properFraction (datum_real_err d)
      in (i,Double j)
  truncate = truncate . datum_real_err
  round = round . datum_real_err
  ceiling = ceiling . datum_real_err
  floor = floor . datum_real_err

instance Ord Datum where
    p < q = case (datum_real p,datum_real q) of
              (Just i,Just j) -> i < j
              _ -> error "datum,ord,partial"

-- | Direct unary 'Int' and 'Double' functions at 'Datum' fields, or
-- 'error'.
--
-- > at_d1 show show (Int 5) == "5"
at_d1 :: (Int -> a) -> (Double -> a) -> Datum -> a
at_d1 fi fr d =
    case d of
      Int n -> fi n
      Float n -> fr n
      Double n -> fr n
      _ -> error "at_d1,partial"

-- | Direct binary 'Int' and 'Double' functions at 'Datum' fields, or
-- 'error'.
at_d2 :: (Int -> Int -> a) ->
         (Double -> Double -> a) ->
         Datum -> Datum -> a
at_d2 fi fr d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> fi n1 n2
      (Float n1,Float n2) -> fr n1 n2
      (Double n1,Double n2) -> fr n1 n2
      _ -> error "at_d2,partial"

-- | Direct ternary 'Int' and 'Double' functions at 'Datum' fields, or
-- 'error'.
at_d3 :: (Int -> Int -> Int -> a) ->
         (Double -> Double -> Double -> a) ->
         Datum -> Datum -> Datum -> a
at_d3 fi fr d1 d2 d3 =
    case (d1,d2,d3) of
      (Int n1,Int n2,Int n3) -> fi n1 n2 n3
      (Float n1,Float n2,Float n3) -> fr n1 n2 n3
      (Double n1,Double n2,Double n3) -> fr n1 n2 n3
      _ -> error "at_d3,partial"

instance Enum Datum where
    fromEnum = at_d1 fromEnum fromEnum
    enumFrom = at_d1 (map Int . enumFrom) (map Double . enumFrom)
    enumFromThen = at_d2 (\a -> map Int . enumFromThen a)
                         (\a -> map Double . enumFromThen a)
    enumFromTo = at_d2 (\a -> map Int . enumFromTo a)
                       (\a -> map Double . enumFromTo a)
    enumFromThenTo = at_d3 (\a b ->  map Int . enumFromThenTo a b)
                           (\a b ->  map Double . enumFromThenTo a b)
    toEnum = Int

instance Random Datum where
  randomR i g =
      case i of
        (Int l,Int r) -> let (n,g') = randomR (l,r) g in (Int n,g')
        (Float l,Float r) -> let (n,g') = randomR (l,r) g in (Float n,g')
        (Double l,Double r) -> let (n,g') = randomR (l,r) g in (Double n,g')
        _ -> error "randomR,datum,partial"
  random g = let (n,g') = randomR (0::Double,1::Double) g in (Double n,g')

{-
5 :: Datum
(5 + 4) :: Datum
(2.0 ** 3.0) :: Datum
(negate 5) :: Datum
(negate 5.0) :: Datum
:set -XOverloadedStrings
"string" :: Datum
-}
