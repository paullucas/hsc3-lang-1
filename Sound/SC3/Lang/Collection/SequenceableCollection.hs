module Sound.SC3.Lang.Collection.SequenceableCollection where

import Control.Monad
import Data.List
import Data.List.Split {- split -}
import Sound.SC3.Lang.Collection.Collection
import System.Random {- random -}
import System.Random.Shuffle {- random-shuffle -}

with_counter :: (a -> (b,a)) -> Int -> a -> [b]
with_counter f =
    let go n i =
            case n of
              0 -> []
              _ -> let (r,i') = f i in r : go n i'
    in go

-- | Arithmetic series (size, start, step)
series :: (Num a) => Int -> a -> a -> [a]
series n i j =
    case n of
      0 -> []
      _ -> i : series (n - 1) (i + j) j

-- | Geometric series (size, start, grow)
geom :: (Num a) => Int -> a -> a -> [a]
geom n i j =
    case n of
      0 -> []
      _ -> i : series (n - 1) (i * j) j

-- | Fibonacci series (size, initial step, start)
fib :: (Num a) => Int -> a -> a -> [a]
fib n i j =
    case n of
      0 -> []
      _ -> j : fib (n - 1) j (i + j)

-- | Random values (size, min, max) - ought this be in floating?
rand :: (Random a) => Int -> a -> a -> IO [a]
rand n l r = replicateM n (getStdRandom (randomR (l, r)))

-- | Random values in the range -abs to +abs (size, abs)
rand2 :: (Num a, Random a) => Int -> a -> IO [a]
rand2 n m = replicateM n (getStdRandom (randomR (negate m, m)))

-- | The first element.
first :: [t] -> Maybe t
first xs =
    case xs of
      [] -> Nothing
      x:_ -> Just x

-- | The last element.
last' :: [t] -> Maybe t
last' xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _:xs' -> last' xs'

-- | flip elemIndex
indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf = flip elemIndex

-- | indexOf
indexOfEqual :: Eq a => [a] -> a -> Maybe Int
indexOfEqual = indexOf

-- | Collection is sorted, index of first greater element.
indexOfGreaterThan :: (Ord a) => a -> [a] -> Maybe Int
indexOfGreaterThan e = detectIndex (ignoringIndex (> e))

-- | Collection is sorted, index of nearest element.
indexIn :: (Ord a, Num a) => a -> [a] -> Int
indexIn e l =
    let f 0 = 0
        f j = let i = j - 1
                  right = l !! j
                  left = l !! i
              in if (e - left) < (right - e) then i else j
    in maybe (size l - 1) f (indexOfGreaterThan e l)

-- | Collection is sorted, linearly interpolated fractional index.
indexInBetween :: (Ord a, Fractional a) => a -> [a] -> a
indexInBetween e l =
    let f 0 = 0
        f j = let i = fromIntegral j
                  a = l !! (j - 1)
                  b = l !! j
                  d = b - a
              in if d == 0 then i else ((e - a) / d) + i - 1
    in maybe (fromIntegral (size l) - 1) f (indexOfGreaterThan e l)

keep :: Int -> [a] -> [a]
keep n l =
    if n < 0
    then drop (length l + n) l
    else take n l

drop' :: Int -> [a] -> [a]
drop' n l =
    if n < 0
    then take (length l + n) l
    else drop n l

extendSequences :: [[a]] -> [[a]]
extendSequences l =
    let n = maximum (map length l)
    in map (take n . cycle) l

flop :: [[a]] -> [[a]]
flop = transpose . extendSequences

choose' :: RandomGen g => [a] -> g -> (a,g)
choose' l g =
    let (i,g') = randomR (0, length l - 1) g
    in (l !! i,g')

choose :: [a] -> IO a
choose l = liftM (l!!) (getStdRandom (randomR (0, length l - 1)))

separateAt :: (a -> a -> Bool) -> [a] -> ([a], [a])
separateAt f xs =
    case xs of
      (x1:x2:xs') ->
          if f x1 x2
          then ([x1], x2:xs')
          else let g e (l,r) = (e:l, r)
               in x1 `g` separateAt f (x2:xs')
      _ -> (xs,[])

separate :: (a -> a -> Bool) -> [a] -> [[a]]
separate f l =
    let (e, r) = separateAt f l
    in if null r then [e] else e : separate f r

clump :: Int -> [a] -> [[a]]
clump = splitEvery

clumps :: [Int] -> [a] -> [[a]]
clumps m s =
    let f [] _ = undefined
        f (n:ns) l = let (e, r) = splitAt n l
                     in if null r then [e] else e : clumps ns r
    in case m of
         [] -> []
         _ -> f (cycle m) s

-- | dx -> d
integrate :: (Num a) => [a] -> [a]
integrate = scanl1 (+)

-- | d -> dx
differentiate :: (Num a) => [a] -> [a]
differentiate l = zipWith (-) l (0:l)

-- | Rotate n places to the left (ie. rotate 1 [1, 2, 3] is [2, 3, 1]).
rotate :: Int -> [a] -> [a]
rotate n p =
    let (b, a) = splitAt n p
    in a ++ b

-- | Ensure sum of elements is one.
normalizeSum :: (Fractional a) => [a] -> [a]
normalizeSum l =
    let n = sum l
    in map (/ n) l

-- | Variant that cycles the shorter input.
zipWith_c :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_c f a b =
    let g [] [] _ = []
        g [] b' (_, e) = if e then [] else g a b' (True, e)
        g a' [] (e, _) = if e then [] else g a' b (e, True)
        g (a0 : aN) (b0 : bN) e = f a0 b0 : g aN bN e
    in g a b (False, False)

zip_c :: [a] -> [b] -> [(a, b)]
zip_c = zipWith_c (,)

scramble' :: Enum a => a -> [t] -> [t]
scramble' j k =
    let g = mkStdGen (fromEnum j)
    in shuffle' k (length k) g

scramble :: [t] -> IO [t]
scramble k = do
    g <- getStdGen
    return (shuffle' k (length k) g)

windex :: (Ord a, Num a) => [a] -> a -> Maybe Int
windex w n = findIndex (n <) (integrate w)

wchoose :: (Random a, Ord a, Fractional a) => [b] -> [a] -> IO b
wchoose l w = do
  i <- randomRIO (0.0,1.0)
  let n = case windex w i of
            Nothing -> error "wchoose: windex"
            Just n' -> n'
  return (l !! n)

{-
sequence (replicate 20 (wchoose [1..5] [0.4,0.2,0.2,0.1,0.1]))
-}
