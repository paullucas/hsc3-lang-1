-- | @sclang@ math functions.
module Sound.SC3.Lang.Math where

import Data.Bits

-- * Binary

-- | @0@ is false, @1@ is True, else error.
--
-- > map bitChar "01" == [False,True]
bitChar :: Char -> Bool
bitChar c =
    case c of
      '0' -> False
      '1' -> True
      _ -> error "bitChar"

-- | Parse a sequence of 0 and 1 characters as a BE bit sequence
--
-- > parseBits "101" == 5
-- > parseBits "00001111" == 15
parseBits :: (Num a,Bits a) => String -> a
parseBits x =
    let x' = filter (id . bitChar . snd) (zip [0..] (reverse x))
    in foldr ((.|.) . bit . fst) 0 x'

-- * SimpleNumber

-- | Variant of @SimpleNumber.exprand@ that shifts a linear (0,1)
-- value to an exponential distribution.
--
-- > map (floor . exprandrng 10 100) [0,0.5,1] == [10,31,100]
exprandrng :: (Floating b) => b -> b -> b -> b
exprandrng l r i = l * exp (log (r / l) * i)

-- | Psuedo-inifite bounded value.
--
-- > inf == maxBound
inf :: Bounded a => a
inf = maxBound

-- | Predicate for 'inf'.
--
-- > isInf inf == True
isInf :: (Eq a,Bounded a) => a -> Bool
isInf = (== inf)

-- | @SimpleNumber.linexp@ shifts from linear to exponential ranges.
--
-- > map (floor . linexp 1 2 10 100) [1,1.5,2] == [10,31,100]
linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp l r l' r' n =
    if n <= l
    then l'
    else if n >= r
         then r'
         else ((r'/l') ** ((n-l)/(r-l))) * l'

-- * Gain

-- | Synonym for 'logBase' @10@.
log10 :: Floating a => a -> a
log10 = logBase 10

-- | RMS to decibel.
--
-- > map (round . rmsToDb) [1,0.7,0.5,0.35,0.25] == [0,-3,-6,-9,-12]
rmsToDb :: Floating a => a -> a
rmsToDb rms = log10 rms * 20

-- | Decibel to RMS.
--
-- > map (round . (* 100) . dbToRms) [0,-3,-6,-9,-12] == [100,71,50,35,25]
dbToRms :: Floating a => a -> a
dbToRms db  = 10 ** (db * 0.05)

-- | Power to decibel.
--
-- > map (round . powToDb) [1,0.5,0.25,0.13,6e-2] == [0,-3,-6,-9,-12]
powToDb :: Floating a => a -> a
powToDb pow = 10 * log10 pow

-- | Decibel to power.
--
-- > map (round . (* 100) . dbToPow) [0,-3,-6,-9,-12] == [100,50,25,13,6]
dbToPow :: Floating a => a -> a
dbToPow db  = 10 ** (db * 0.1)
