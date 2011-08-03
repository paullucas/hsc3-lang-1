module Sound.SC3.Lang.Math.Binary where

import Data.Bits

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
    in foldr1 (.|.) (map (bit . fst) x')

{-
(parseBits "101")::Int
(parseBits "00001111")::Int
-}
