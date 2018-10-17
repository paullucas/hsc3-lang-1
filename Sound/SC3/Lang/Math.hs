-- | @sclang@ math functions.
module Sound.SC3.Lang.Math where

import Data.Bits {- base -}

import qualified Sound.SC3.Common.Math as SC3 {- hsc3 -}

-- * SimpleNumber

-- | @SimpleNumber.ampdb@ converts linear amplitude to decibels.
--
-- > > [1,0.5,0.25,0.13,6e-2].collect({|i| i.ampdb.round}) == [0,-6,-12,-18,-24]
-- > map (round . ampdb) [1,0.5,0.25,0.13,6e-2] == [0,-6,-12,-18,-24]
--
-- > > [1,0.7,0.5,0.35,0.25].collect({|i| i.ampdb.round}) == [0,-3,-6,-9,-12]
-- > map (round . ampdb) [1,0.7,0.5,0.35,0.25] == [0,-3,-6,-9,-12]
ampdb :: Floating a => a -> a
ampdb = SC3.amp_to_db

-- | @SimpleNumber.dbamp@ converts decibels to a linear amplitude.
--
-- > > [0,-3,-6,-9,-12].collect({|i| (i.dbamp * 100).floor}) == [100,70,50,35,25]
-- > map (floor . (* 100) . dbamp) [0,-3,-6,-9,-12] == [100,70,50,35,25]
dbamp :: Floating a => a -> a
dbamp = SC3.db_to_amp

-- | @SimpleNumber.degreeToKey@ translates degree, scale and steps per
-- octave to key.
--
-- > > (0..5).collect({|i| i.degreeToKey([0,1,5,9,11],12)}) == [0,1,5,9,11,12]
-- > map (degreeToKey [0,1,5,9,11] 12) [0..5] == [0,1,5,9,11,12]
--
-- > map (degreeToKey [0,2,4,5,7,9,11] 12) [5,6,7,8] == [9,11,12,14]
degreeToKey :: (RealFrac a) => [a] -> a -> a -> a
degreeToKey = SC3.degree_to_key

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

-- | @SimpleNumber.log10@ is the base 10 logarithm.
log10 :: Floating a => a -> a
log10 = logBase 10

octpc_to_midi :: Num a => (a,a) -> a
octpc_to_midi (o,pc) = 60 + ((o - 4) * 12) + pc

-- | @SimpleNumber.midicps@ translates from midi note number to cycles
-- per second.
--
-- > > [57,69].collect({|i| i.midicps}) == [220,440]
-- > map midicps [57,69] == [220,440]
midicps :: (Floating a) => a -> a
midicps = SC3.midi_to_cps

-- | 'midicps' of 'octpc_to_midi'.
octpc_to_cps :: (Floating a) => (a,a) -> a
octpc_to_cps = midicps . octpc_to_midi

-- | 'octpc_to_cps' of 'degreeToKey'.
degree_to_cps :: (Floating a, RealFrac a) => [a] -> a -> a -> a -> a
degree_to_cps sc n d o =
    let pc = degreeToKey sc n d
    in octpc_to_cps (o,pc)

-- | Variant with list inputs for degree and octave, and scalar inputs for scale and steps.
degree_to_cps' :: (Floating a, RealFrac a) => [a] -> a -> [a] -> [a] -> [a]
degree_to_cps' sc n = zipWith (degree_to_cps sc n)

-- * UGen

-- | @UGen.exprand@ shifts a linear (0,1) value to an exponential
-- range.
--
-- > map (floor . exprange 10 100) [0,0.5,1] == [10,31,100]
exprange :: (Floating b) => b -> b -> b -> b
exprange l r i = l * exp (log (r / l) * i)

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
