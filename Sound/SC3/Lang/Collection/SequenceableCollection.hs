module Sound.SC3.Lang.Collection.SequenceableCollection where

import Prelude
import Control.Monad
import Data.List
import System.Random
import Sound.SC3.Lang.Collection.Collection

-- | Arithmetic series (size, start, step)
series :: (Num a) => Int -> a -> a -> [a]
series 0 _ _ = []
series n i j = i : series (n - 1) (i + j) j

-- | Geometric series (size, start, grow)
geom :: (Num a) => Int -> a -> a -> [a]
geom 0 _ _ = []
geom n i j = i : series (n - 1) (i * j) j

-- | Fibonacci series (size, initial step, start)
fib :: (Num a) => Int -> a -> a -> [a]
fib 0 _ _ = []
fib n i j = j : fib (n - 1) j (i + j)

-- | Random values (size, min, max) - ought this be in floating?
rand :: (Random a) => Int -> a -> a -> IO [a]
rand n l r = replicateM n (getStdRandom (randomR (l, r)))

-- | Random values in the range -abs to +abs (size, abs)
rand2 :: (Num a, Random a) => Int -> a -> IO [a]
rand2 n m = replicateM n (getStdRandom (randomR (negate m, m)))

-- | The first element.
first :: [t] -> Maybe t
first (x:_) = Just x
first _ = Nothing

-- | The last element.
last' :: [t] -> Maybe t
last' [] = Nothing
last' [x] = Just x
last' (_:xs) = last' xs

-- | flip elemIndex
indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf = flip elemIndex

-- | indexOf
indexOfEqual :: Eq a => [a] -> a -> Maybe Int
indexOfEqual = indexOf

-- | Collection is sorted, index of first greater element.
indexOfGreaterThan :: (Ord a) => a -> [a] -> Maybe Int
indexOfGreaterThan e l = detectIndex (ignoringIndex (> e)) l

-- | Collection is sorted, index of nearest element.
indexIn :: (Ord a, Num a) => a -> [a] -> Int
indexIn e l = maybe (size l - 1) f (indexOfGreaterThan e l)
    where f 0 = 0
          f j = if (e - left) < (right - e) then i else j 
              where i = j - 1
                    right = l !! j
                    left = l !! i

-- | Collection is sorted, linearly interpolated fractional index.
indexInBetween :: (Ord a, Fractional a) => a -> [a] -> a
indexInBetween e l = maybe (fromIntegral (size l) - 1) f (indexOfGreaterThan e l)
    where f 0 = 0
          f j = if d == 0 then i else ((e - a) / d) + i - 1
              where i = fromIntegral j
                    a = l !! (j - 1)
                    b = l !! j
                    d = b - a

keep :: Int -> [a] -> [a]
keep n l | n < 0 = maybe l id (find (\e -> length e == negate n) (tails l))
         | otherwise = take n l

drop' :: Int -> [a] -> [a]
drop' n l | n < 0 = take (length l + n) l
          | otherwise = drop n l

extendSequences :: [[a]] -> [[a]]
extendSequences l = map (take n . cycle) l
    where n = maximum (map length l)

flop :: [[a]] -> [[a]]
flop = transpose . extendSequences

choose :: [a] -> IO a
choose l = liftM (l!!) (getStdRandom (randomR (0, length l - 1)))

separateAt :: (a -> a -> Bool) -> [a] -> ([a], [a])
separateAt f (x1:x2:xs) = if f x1 x2 
                          then ([x1], x2:xs) 
                          else x1 `g` separateAt f (x2:xs)
                              where g e (l,r) = (e:l, r)
separateAt _ l = (l,[])

separate :: (a -> a -> Bool) -> [a] -> [[a]]
separate f l = if null r then [e] else e : separate f r
    where (e, r) = separateAt f l

clump :: Int -> [a] -> [[a]]
clump n l = if null r then [e] else e : clump n r
    where (e, r) = splitAt n l

clumps :: [Int] -> [a] -> [[a]]
clumps m s = f (cycle m) s
    where f [] _ = undefined
          f (n:ns) l = if null r then [e] else e :clumps ns r
              where (e, r) = splitAt n l

-- | dx -> d
integrate :: (Num a) => [a] -> [a]
integrate [] = []
integrate (x:xs) = x : snd (mapAccumL f x xs)
    where f p c = (p + c, p + c)

-- | d -> dx
differentiate :: (Num a) => [a] -> [a]
differentiate l = zipWith (-) l (0:l)
