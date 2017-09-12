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
-- 'Datum' are 'EqE'
--
-- > Int32 5 /=* Int32 6 == Int32 1
-- > Double 5 ==* Double 5 == Double 1
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
-- > sqrt (Int32 4) == Double 2
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
-- 'Datum' are 'RealFracE'
--
-- > roundE (Double 1.4) == Double 1
-- > ceilingE (Double 1.4) == Double 2
--
-- 'Datum' are 'RealFloat'
--
-- > isNaN (sqrt (negate (Int32 1))) == True
--
-- 'Datum' are 'Ord'
--
-- > Double 7.5 > Int32 7
-- > string "because" > string "again"
--
-- 'Datum' are 'OrdE'
--
-- > Int32 7 >* Int32 7 == Int32 0
-- > Double 7.5 >* Int32 7 == Double 1
--
-- 'Datum' are 'Enum'
--
-- > [Int32 0 .. Int32 4] == [Int32 0,Int32 1,Int32 2,Int32 3,Int32 4]
-- > [Double 1 .. Double 3] == [Double 1,Double 2,Double 3]
--
-- 'Datum' are 'Random'
--
-- > System.Random.randomRIO (Int32 0,Int32 9):: IO Datum
-- > System.Random.randomRIO (Float 0,Float 1):: IO Datum
module Sound.SC3.Lang.Collection.Universal.Datum where

import Prelude hiding ((<*))
import qualified Data.ByteString.Char8 as C {- bytestring -}
import Data.Int {- base -}
import Data.Ratio {- base -}
import Data.String {- base -}
import System.Random {- random -}

import Sound.OSC {- hosc -}
import Sound.OSC.Datum.Normalise {- hosc -}
import Sound.SC3 {- hsc3 -}

-- * Lifting

-- | Unary operator.
type UOp n = (n -> n)

-- | Lift an equivalent set of 'Int32', 'Int64', 'Float' and 'Double' unary
-- functions to 'Datum'.
--
-- > map (liftD abs abs abs abs) [Int32 5,Float (-5)] == [Int32 5,Float 5]
liftD :: UOp Int32 -> UOp Int64 -> UOp Float -> UOp Double -> UOp Datum
liftD fi fh ff fd d =
    case d of
      Int32 n -> Int32 (fi n)
      Int64 n -> Int64 (fh n)
      Float n -> Float (ff n)
      Double n -> Double (fd n)
      _ -> error "liftD: NaN"

-- | Lift a 'Double' unary operator to 'Datum' via 'datum_promote'.
--
-- > liftD' negate (Int 5) == Double (-5)
liftD' :: UOp Double -> UOp Datum
liftD' fd =
    liftD (error "liftD'") (error "liftD'") (error "liftD'") fd .
    datum_promote

-- | A binary operator.
type BinOp n = (n -> n -> n)

-- | Given 'Int32', 'Int64', 'Float' and 'Double' binary operators
-- generate 'Datum' operator.  If 'Datum' are of equal type result
-- type is equal, else result type is 'Double'.
--
-- > liftD2 (+) (+) (+) (+) (Float 1) (Float 2) == Float 3
-- > liftD2 (*) (*) (*) (*) (Int32 3) (Float 4) == Double 12
liftD2 :: BinOp Int32 -> BinOp Int64 ->
          BinOp Float -> BinOp Double ->
          BinOp Datum
liftD2 fi fh ff fd d1 d2 =
    case (d1,d2) of
      (Int32 n1,Int32 n2) -> Int32 (fi n1 n2)
      (Int64 n1,Int64 n2) -> Int64 (fh n1 n2)
      (Float n1,Float n2) -> Float (ff n1 n2)
      (Double n1,Double n2) -> Double (fd n1 n2)
      _ -> case (datum_floating d1,datum_floating d2) of
             (Just n1,Just n2) -> Double (fd n1 n2)
             _ -> error "liftD2: NaN"

-- | A 'datum_promote' variant of 'liftD2'.
--
-- > liftD2' (+) (Float 1) (Float 2) == Double 3
liftD2' :: BinOp Double -> BinOp Datum
liftD2' f d1 =
    let d1' = datum_promote d1
    in liftD2 (error "liftD2'") (error "liftD2'") (error "liftD2'") f d1' .
       datum_promote

-- * At

-- | Direct unary 'Int32', 'Int64', 'Float' and 'Double' functions at
-- 'Datum' fields, or 'error'.
--
-- > atD show show show show (Int 5) == "5"
atD :: (Int32 -> a) -> (Int64 -> a) ->
       (Float -> a) -> (Double -> a) ->
       Datum -> a
atD fi fh ff fd d =
    case d of
      Int32 n -> fi n
      Int64 n -> fh n
      Float n -> ff n
      Double n -> fd n
      _ -> error "atD: NaN"

-- | Lift a 'Double' /at/ operator to 'Datum' via 'datum_promote'.
--
-- > atD' floatRadix (Int 5) == 2
atD' :: (Double -> a) -> Datum -> a
atD' f = f . d_double . datum_promote

-- | Binary /at/ function.
type BinAt n a = (n -> n -> a)

-- | Direct binary 'Int', 'Float' and 'Double' functions at 'Datum'
-- fields, or 'error'.
atD2 :: BinAt Int32 a -> BinAt Int64 a ->
        BinAt Float a -> BinAt Double a ->
        BinAt Datum a
atD2 fi fh ff fd d1 d2 =
    case (d1,d2) of
      (Int32 n1,Int32 n2) -> fi n1 n2
      (Int64 n1,Int64 n2) -> fh n1 n2
      (Float n1,Float n2) -> ff n1 n2
      (Double n1,Double n2) -> fd n1 n2
      _ -> error "atD2: NaN"

-- | Ternary /at/ function.
type TriAt n a = (n -> n -> n -> a)

-- | Direct ternary 'Int', 'Float' and 'Double' functions at 'Datum'
-- fields, or 'error'.
atD3 :: TriAt Int32 a -> TriAt Int64 a ->
        TriAt Float a -> TriAt Double a ->
        TriAt Datum a
atD3 fi fh ff fd d1 d2 d3 =
    case (d1,d2,d3) of
      (Int32 n1,Int32 n2,Int32 n3) -> fi n1 n2 n3
      (Int64 n1,Int64 n2,Int64 n3) -> fh n1 n2 n3
      (Float n1,Float n2,Float n3) -> ff n1 n2 n3
      (Double n1,Double n2,Double n3) -> fd n1 n2 n3
      _ -> error "atD3: NaN"

instance IsString Datum where
    fromString = ASCII_String . C.pack

instance EqE Datum where
    (==*) = liftD2 (==*) (==*) (==*) (==*)
    (/=*) = liftD2 (/=*) (/=*) (/=*) (/=*)

instance Num Datum where
    negate = liftD negate negate negate negate
    (+) = liftD2 (+) (+) (+) (+)
    (-) = liftD2 (-) (-) (-) (-)
    (*) = liftD2 (*) (*) (*) (*)
    abs = liftD abs abs abs abs
    signum = liftD signum signum signum signum
    fromInteger n = Int64 (fromInteger n)

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
          Int32 n -> fromIntegral n % 1
          Int64 n -> fromIntegral n % 1
          Float n -> toRational n
          Double n -> toRational n
          _ -> error "Datum.toRational: NaN"

instance RealFrac Datum where
  properFraction d =
      let (i,j) = properFraction (d_double d)
      in (i,Double j)
  truncate = atD' truncate
  round = atD' round
  ceiling = atD' ceiling
  floor = atD' floor

instance RealFracE Datum where
  truncateE = liftD undefined undefined truncateE truncateE
  roundE = liftD undefined undefined roundE roundE
  ceilingE = liftD undefined undefined ceilingE ceilingE
  floorE = liftD undefined undefined floorE floorE

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
    compare p q =
        case (datum_promote p,datum_promote q) of
          (Double i, Double j) -> compare i j
          (ASCII_String i,ASCII_String j) -> compare i j
          (TimeStamp i,TimeStamp j) -> compare i j
          _ -> error "Datum.compare"

instance OrdE Datum where
    (>*) = liftD2 (>*) (>*) (>*) (>*)
    (>=*) = liftD2 (>=*) (>=*) (>=*) (>=*)
    (<*) = liftD2 (<*) (<*) (<*) (<*)
    (<=*) = liftD2 (<=*) (<=*) (<=*) (<=*)

instance Enum Datum where
    fromEnum = atD fromEnum fromEnum fromEnum fromEnum
    enumFrom =
        atD
        (map Int32 . enumFrom)
        (map Int64 . enumFrom)
        (map Float . enumFrom)
        (map Double . enumFrom)
    enumFromThen =
        atD2
        (\a -> map Int32 . enumFromThen a)
        (\a -> map Int64 . enumFromThen a)
        (\a -> map Float . enumFromThen a)
        (\a -> map Double . enumFromThen a)
    enumFromTo =
        atD2
        (\a -> map Int32 . enumFromTo a)
        (\a -> map Int64 . enumFromTo a)
        (\a -> map Float . enumFromTo a)
        (\a -> map Double . enumFromTo a)
    enumFromThenTo =
        atD3
        (\a b ->  map Int32 . enumFromThenTo a b)
        (\a b ->  map Int64 . enumFromThenTo a b)
        (\a b ->  map Float . enumFromThenTo a b)
        (\a b ->  map Double . enumFromThenTo a b)
    toEnum = Int64 . fromIntegral

instance Random Datum where
  randomR i g =
      case i of
        (Int32 l,Int32 r) -> let (n,g') = randomR (l,r) g in (Int32 n,g')
        (Int64 l,Int64 r) -> let (n,g') = randomR (l,r) g in (Int64 n,g')
        (Float l,Float r) -> let (n,g') = randomR (l,r) g in (Float n,g')
        (Double l,Double r) -> let (n,g') = randomR (l,r) g in (Double n,g')
        _ -> error "Datum.randomR: NaN"
  random g = let (n,g') = randomR (0::Double,1::Double) g in (Double n,g')
