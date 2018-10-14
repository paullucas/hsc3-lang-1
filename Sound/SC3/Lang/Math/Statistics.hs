-- | Simple statistical measures.
module Sound.SC3.Lang.Math.Statistics where

import Data.List {- base -}

-- | Mean (average).
--
-- > mean [1..9] == 5
-- > round (mean [1,2,3,5,8,12]) == 5
mean :: Floating a => [a] -> a
mean = fst . foldl' (\(m,n) x -> (m + (x - m) / (n + 1),n + 1)) (0,0)

-- | Harmonic mean.
--
-- > round (harmonic_mean [1..9]) == 3
harmonic_mean :: (Floating a) => [a] -> a
harmonic_mean xs = fromIntegral (length xs) / (sum (map (1 /) xs))

-- | Geometric mean.
--
-- > round (geometric_mean [1..9]) == 4
geometric_mean :: (Floating a) => [a] -> a
geometric_mean xs = (foldr1 (*) xs) ** (1 / fromIntegral (length xs))

-- | Median
--
-- > median [1..9] == 5
-- > median [1,2,3,5,8,12] == 4
median :: (Floating a, Ord a) => [a] -> a
median xs =
    let xs' = sort xs
        n  = length xs
        i = (n `div` 2) - 1
    in if odd n
       then head (drop (n `div` 2) xs')
       else mean (take 2 (drop i xs'))

-- | Select averaging function by name.
--
-- > parse_averaging_f "median" [1,2,3,5,8,12] == 4
parse_averaging_f :: (Floating n,Ord n) => String -> [n] -> n
parse_averaging_f nm =
    case nm of
      "median" -> median
      "mean" -> mean
      "harmonic-mean" -> harmonic_mean
      "geometric-mean" -> geometric_mean
      _ -> error "parse_avg_f"
