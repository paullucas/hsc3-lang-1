module Sound.SC3.Lang.Math where

import Data.Bits

-- * Binary

-- | 0 is false, 1 is True, else error.
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
parseBits :: Bits a => String -> a
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
