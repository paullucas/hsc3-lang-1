module Sound.SC3.Lang.Math.Datum where

import Data.Maybe
import Data.Ratio
import GHC.Exts (IsString(..))
import Sound.OpenSoundControl.Type (Datum(..))
import System.Random

instance IsString Datum where
    fromString = String

datum_r :: Datum -> Maybe Double
datum_r d =
    case d of
      Double n -> Just n
      Float n -> Just n
      Int n -> Just (fromIntegral n)
      _ -> Nothing

datum_r' :: Datum -> Double
datum_r' = fromJust . datum_r

datum_str :: Datum -> Maybe String
datum_str d =
    case d of
      String s -> Just s
      _ -> Nothing

datum_str' :: Datum -> String
datum_str' = fromJust . datum_str

datum_lift :: (Int -> Int) -> (Double -> Double) -> (Datum -> Datum)
datum_lift fi fd d =
    case d of
      Int n -> Int (fi n)
      Float n -> Float (fd n)
      Double n -> Double (fd n)
      _ -> error "datum_lift"

datum_promote :: Datum -> Datum
datum_promote d =
    case d of
      Int n -> Double (fromIntegral n)
      Float n -> Double n
      _ -> d

datum_lift' :: (Double -> Double) -> (Datum -> Datum)
datum_lift' f = datum_lift (error "datum_lift:non integral") f .
                datum_promote

type I_Binop = Int -> Int -> Int
type F_Binop = Double -> Double -> Double

datum_lift2 :: I_Binop -> F_Binop -> (Datum -> Datum -> Datum)
datum_lift2 fi fd d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> Int (fi n1 n2)
      (Float n1,Float n2) -> Float (fd n1 n2)
      (Double n1,Double n2) -> Double (fd n1 n2)
      _ -> case (datum_r d1,datum_r d2) of
             (Just n1,Just n2) -> Double (fd n1 n2)
             _ -> error "datum_lift2"

datum_lift2' :: F_Binop -> (Datum -> Datum -> Datum)
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
  properFraction d = let (i,j) = properFraction (datum_r' d) in (i,Double j)
  truncate = truncate . datum_r'
  round = round . datum_r'
  ceiling = ceiling . datum_r'
  floor = floor . datum_r'

instance Ord Datum where
    p < q = case (datum_r p,datum_r q) of
              (Just i,Just j) -> i < j
              _ -> error "datum,ord,partial"

at_d1 :: (Int -> a) -> (Double -> a) -> Datum -> a
at_d1 fi fr d =
    case d of
      Int n -> fi n
      Float n -> fr n
      Double n -> fr n
      _ -> error "at_d1,partial"

at_d2 :: (Int -> Int -> a) ->
         (Double -> Double -> a) ->
         Datum -> Datum -> a
at_d2 fi fr d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> fi n1 n2
      (Float n1,Float n2) -> fr n1 n2
      (Double n1,Double n2) -> fr n1 n2
      _ -> error "at_d2,partial"

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
    toEnum n = Int n

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
