{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Functions to allow using the "Sound.OpenSoundControl" 'Datum' as
-- a /universal/ data type.  In addition to the functions defined
-- below it provides instances for:
--
-- 'Datum' are 'IsString'
--
-- > :set -XOverloadedStrings
-- > "string" :: Datum
--
-- 'Datum' are 'Num'
--
-- > 5 :: Datum
-- > 5 + 4 :: Datum
-- > negate 5 :: Datum
--
-- 'Datum' are 'Fractional'
--
-- > 5.0 :: Datum
-- > (5 / 4) :: Datum
--
-- 'Datum' are 'Floating'
--
-- > pi :: Datum
-- > sqrt (Int 4) == Double 2
-- > (2.0 ** 3.0) :: Datum
--
-- 'Datum' are 'Real'
--
-- > toRational (Double 1.5) == (3/2 :: Rational)
-- > (realToFrac (1.5 :: Double) :: Datum) == Double 1.5
-- > (realToFrac (Double 1.5) :: Datum) == Double 1.5
-- > (realToFrac (Double 1.5) :: Double) == 1.5
--
-- 'Datum' are 'RealFrac'
--
-- > round (Double 1.4) == 1
--
-- 'Datum' are 'RealFloat'
--
-- > isNaN (sqrt (negate (Int 1))) == True
--
-- 'Datum' are 'Ord'
--
-- > Double 7.5 > Int 7
--
-- 'Datum' are 'Enum'
--
-- > [Int 0 .. Int 4] == [Int 0,Int 1,Int 2,Int 3,Int 4]
-- > [Double 1 .. Double 3] == [Double 1,Double 2,Double 3]
--
-- 'Datum' are 'Random'
--
-- > System.Random.randomRIO (Int 0,Int 9):: IO Datum
module Sound.SC3.Lang.Collection.Universal.Datum where

import Data.Maybe {- base -}
import Data.Ratio {- base -}
import GHC.Exts (IsString(..)) {- base -}
import Sound.OSC {- hosc -}
import System.Random {- random -}

-- * Cast, Coerce, Promote

-- | Promote 'Int' and 'Float' 'Datum' to 'Double' 'Datum'.
--
-- > map datum_promote [Int 5,Float 5] == [Double 5,Double 5]
datum_promote :: Datum -> Datum
datum_promote d =
    case d of
      Int n -> Double (fromIntegral n)
      Float n -> Double (realToFrac n)
      _ -> d

-- * Lifting

type UOp n = (n -> n)

-- | Lift an equivalent set of 'Int', 'Float' and 'Double' unary
-- functions to 'Datum'.
--
-- > map (liftD negate negate) [Int 5,Float 5] == [Int (-5),Float (-5)]
liftD :: UOp Int -> UOp Float -> UOp Double -> UOp Datum
liftD fi ff fd d =
    case d of
      Int n -> Int (fi n)
      Float n -> Float (ff n)
      Double n -> Double (fd n)
      _ -> error "liftD: non numerical"

-- | Lift a 'Float' and 'Double' unary operator to 'Datum' via
-- 'datum_promote'.
--
-- > liftD' negate (Int 5) == Double (-5)
liftD' :: UOp Double -> UOp Datum
liftD' fd = liftD (error "liftD'") (error "liftD'") fd . datum_promote

-- | A binary operator.
type BinOp n = (n -> n -> n)

-- | Given 'Int', 'Float' and 'Double' binary operators generate
-- 'Datum' operator.  If 'Datum' are of equal type result type is
-- equal, else result type is 'Double'.
--
-- > liftD2 (+) (+) (+) (Float 1) (Float 2) == Float 3
-- > liftD2 (*) (*) (*) (Int 3) (Float 4) == Double 12
liftD2 :: BinOp Int -> BinOp Float -> BinOp Double -> BinOp Datum
liftD2 fi ff fd d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> Int (fi n1 n2)
      (Float n1,Float n2) -> Float (ff n1 n2)
      (Double n1,Double n2) -> Double (fd n1 n2)
      _ -> case (datum_floating d1,datum_floating d2) of
             (Just n1,Just n2) -> Double (fd n1 n2)
             _ -> error "liftD2: non numerical"

-- | A 'datum_promote' variant of 'liftD2'.
--
-- > liftD2' (+) (Float 1) (Float 2) == Double 3
liftD2' :: BinOp Double -> BinOp Datum
liftD2' f d1 =
    let d1' = datum_promote d1
    in liftD2 (error "liftD2'") (error "liftD2'") f d1' . datum_promote

-- * At

-- | Direct unary 'Int' and 'Double' functions at 'Datum' fields, or
-- 'error'.
--
-- > atD show show (Int 5) == "5"
atD :: (Int -> a) -> (Float -> a) -> (Double -> a) -> Datum -> a
atD fi ff fd d =
    case d of
      Int n -> fi n
      Float n -> ff n
      Double n -> fd n
      _ -> error "atD: partial"

-- | Lift a 'Double' unary operator to 'Datum' via 'datum_promote'.
--
-- > atD' floatRadix (Int 5) == 2
atD' :: (Double -> a) -> Datum -> a
atD' f = f . d_double . datum_promote

type BinAt n a = (n -> n -> a)

-- | Direct binary 'Int' and 'Double' functions at 'Datum' fields, or
-- 'error'.
atD2 :: BinAt Int a -> BinAt Float a -> BinAt Double a -> BinAt Datum a
atD2 fi ff fd d1 d2 =
    case (d1,d2) of
      (Int n1,Int n2) -> fi n1 n2
      (Float n1,Float n2) -> ff n1 n2
      (Double n1,Double n2) -> fd n1 n2
      _ -> error "atD2: partial"

type TriAt n a = (n -> n -> n -> a)

-- | Direct ternary 'Int' and 'Double' functions at 'Datum' fields, or
-- 'error'.
atD3 :: TriAt Int a -> TriAt Float a -> TriAt Double a -> TriAt Datum a
atD3 fi ff fd d1 d2 d3 =
    case (d1,d2,d3) of
      (Int n1,Int n2,Int n3) -> fi n1 n2 n3
      (Float n1,Float n2,Float n3) -> ff n1 n2 n3
      (Double n1,Double n2,Double n3) -> fd n1 n2 n3
      _ -> error "atD3: partial"

instance IsString Datum where
    fromString = String

instance Num Datum where
    negate = liftD negate negate negate
    (+) = liftD2 (+) (+) (+)
    (-) = liftD2 (-) (-) (-)
    (*) = liftD2 (*) (*) (*)
    abs = liftD abs abs abs
    signum = liftD signum signum signum
    fromInteger n = Int (fromInteger n)

instance Fractional Datum where
    recip = liftD' recip
    (/) = liftD2' (/)
    fromRational n = Double (fromRational n)

instance Floating Datum where
    pi = Double pi
    exp = liftD' exp
    log = liftD' log
    sqrt = liftD' sqrt
    (**) = liftD2' (**)
    logBase = liftD2' logBase
    sin = liftD' sin
    cos = liftD' cos
    tan = liftD' tan
    asin = liftD' asin
    acos = liftD' acos
    atan = liftD' atan
    sinh = liftD' sinh
    cosh = liftD' cosh
    tanh = liftD' tanh
    asinh = liftD' asinh
    acosh = liftD' acosh
    atanh = liftD' atanh

instance Real Datum where
    toRational d =
        case d of
          Int n -> fromIntegral n % 1
          Float n -> toRational n
          Double n -> toRational n
          _ -> error "datum,real: partial"

datum_floating_err :: Floating n => Datum -> n
datum_floating_err = fromJust . datum_floating

instance RealFrac Datum where
  properFraction d =
      let (i,j) = properFraction (d_double d)
      in (i,Double j)
  truncate = atD' truncate
  round = atD' round
  ceiling = atD' ceiling
  floor = atD' floor

instance RealFloat Datum where
    floatRadix = atD' floatRadix
    floatDigits = atD' floatDigits
    floatRange = atD' floatRange
    decodeFloat = atD' decodeFloat
    encodeFloat i = Double . encodeFloat i
    exponent = atD' exponent
    significand = liftD' significand
    scaleFloat i = liftD' (scaleFloat i)
    isNaN = atD' isNaN
    isInfinite = atD' isInfinite
    isDenormalized = atD' isDenormalized
    isNegativeZero = atD' isNegativeZero
    isIEEE = atD' isIEEE
    atan2 = liftD2' atan2

instance Ord Datum where
    compare p q = case (datum_double p,datum_double q) of
                    (Just i,Just j) -> compare i j
                    _ -> error "datum,ord: partial"

instance Enum Datum where
    fromEnum = atD fromEnum fromEnum fromEnum
    enumFrom =
        atD
        (map Int . enumFrom)
        (map Float . enumFrom)
        (map Double . enumFrom)
    enumFromThen =
        atD2
        (\a -> map Int . enumFromThen a)
        (\a -> map Float . enumFromThen a)
        (\a -> map Double . enumFromThen a)
    enumFromTo =
        atD2
        (\a -> map Int . enumFromTo a)
        (\a -> map Float . enumFromTo a)
        (\a -> map Double . enumFromTo a)
    enumFromThenTo =
        atD3
        (\a b ->  map Int . enumFromThenTo a b)
        (\a b ->  map Float . enumFromThenTo a b)
        (\a b ->  map Double . enumFromThenTo a b)
    toEnum = Int

instance Random Datum where
  randomR i g =
      case i of
        (Int l,Int r) -> let (n,g') = randomR (l,r) g in (Int n,g')
        (Float l,Float r) -> let (n,g') = randomR (l,r) g in (Float n,g')
        (Double l,Double r) -> let (n,g') = randomR (l,r) g in (Double n,g')
        _ -> error "randomR,datum: partial"
  random g = let (n,g') = randomR (0::Double,1::Double) g in (Double n,g')
