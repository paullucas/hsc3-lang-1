module Sound.SC3.Lang.Math where

import Data.Bits

-- * Binary

-- | 0 is false, 1 is True, else error
bitChar :: Char -> Bool
bitChar c =
    case c of
      '0' -> False
      '1' -> True
      _ -> error "bitChar"

-- | Parse a sequence of 0 and 1 characters as a BE bit sequence
parseBits :: Bits a => String -> a
parseBits x =
    let x' = filter (id . bitChar . snd) (zip [0..] (reverse x))
    in foldr (.|.) 0 (map (bit . fst) x')

{-
(parseBits "101")::Int
(parseBits "00001111")::Int
-}

-- * SimpleNumber

exprandrng :: (Floating b) => b -> b -> b -> b
exprandrng l r i = l * exp (log (r / l) * i)

inf :: Bounded a => a
inf = maxBound

isInf :: (Eq a,Bounded a) => a -> Bool
isInf = (== inf)

linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp l r l' r' n =
    if n <= l
    then l'
    else if n >= r
         then r'
         else ((r'/l') ** ((n-l)/(r-l))) * l'

linexp_ :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp_ n l r l' r' = linexp l r l' r' n
