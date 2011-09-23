-- | In cases where a method takes arguments, these precede the
-- collection argument in the haskell variant, so that @c.m(i,j)@
-- becomes @m i j c@.

module Sound.SC3.Lang.Collection where

import Data.List.Split {- split -}
import Data.List as L
import Data.Maybe

-- * Collection

-- | Collection.*fill is 'map' over indices to /n/.
--
-- > fill 4 (* 2) == [0,2,4,6]
fill :: Int -> (Int -> a) -> [a]
fill n f = map f [0 .. n - 1]

-- | Collection.size is 'length'.
--
-- > size [1,2,3,4] == 4
size :: [a] -> Int
size = length

-- | Collection.isEmpty is 'null'.
--
-- > isEmpty [] == True
isEmpty :: [a] -> Bool
isEmpty = null

-- | Utility equal to 'const' of /f/ of /e/.
--
-- > select (ignoringIndex even) [1,2,3,4] == [2,4]
ignoringIndex :: (a -> b) -> a -> Int -> b
ignoringIndex f e = const (f e)

-- | Collection.collect is 'map' with element indices.
--
-- > collect (\i _ -> i + 10) [1,2,3,4] == [11,12,13,14]
-- > collect (\_ j -> j + 11) [1,2,3,4] == [11,12,13,14]
collect :: (a -> Int -> b) -> [a] -> [b]
collect f l = zipWith f l [0..]

-- | Collection.select is 'filter' with element indices.
--
-- > select (\i _ -> even i) [1,2,3,4] == [2,4]
-- > select (\_ j -> even j) [1,2,3,4] == [1,3]
select :: (a -> Int -> Bool) -> [a] -> [a]
select f l = map fst (filter (uncurry f) (zip l [0..]))

-- | Collection.reject is negated 'filter' with element indices.
--
-- > reject (\i _ -> even i) [1,2,3,4] == [1,3]
-- > reject (\_ j -> even j) [1,2,3,4] == [2,4]
reject :: (a -> Int -> Bool) -> [a] -> [a]
reject f l = map fst (filter (not . uncurry f) (zip l [0..]))

-- | Collection.detect is 'first' '.' 'select'.
--
-- > detect (\i _ -> even i) [1,2,3,4] == Just 2
detect :: (a -> Int -> Bool) -> [a] -> Maybe a
detect f l = maybe Nothing (Just . fst) (find (uncurry f) (zip l [0..]))

-- | Collection.detectIndex is the index locating variant of 'detect'.
--
-- > detectIndex (\i _ -> even i) [1,2,3,4] == Just 1
detectIndex :: (a -> Int -> Bool) -> [a] -> Maybe Int
detectIndex f l = maybe Nothing (Just . snd) (find (uncurry f) (zip l [0..]))

-- | Collection.inject is a variant on 'foldl'.
--
-- > inject 0 (+) [1..5] == 15
-- > inject 1 (*) [1..5] == 120
inject :: a -> (a -> b -> a) -> [b] -> a
inject i f = foldl f i

-- | Collection.any is 'True' if 'detect' is not 'Nothing'.
--
-- > any' (\i _ -> even i) [1,2,3,4] == True
any' :: (a -> Int -> Bool) -> [a] -> Bool
any' f = isJust . detect f

-- | Collection.every is 'True' if /f/ applies at all elements.
--
-- > every (\i _ -> even i) [1,2,3,4] == False
every :: (a -> Int -> Bool) -> [a] -> Bool
every f =
    let g e = not . f e
    in not . any' g

-- | Collection.count is 'length' '.' 'select'.
--
-- > count (\i _ -> even i) [1,2,3,4] == 2
count :: (a -> Int -> Bool) -> [a] -> Int
count f = length . select f

-- | Collection.occurencesOf is an '==' variant of 'count'.
--
-- > occurencesOf 2 [1,2,3,4] == 1
-- > occurencesOf 't' "test" == 2
occurencesOf :: (Eq a) => a -> [a] -> Int
occurencesOf k = count (\e _ -> e == k)

-- | Collection.sum is 'sum' '.' 'collect'.
--
-- > sum' (ignoringIndex (* 2)) [1,2,3,4] == 20
sum' :: (Num a) => (b -> Int -> a) -> [b] -> a
sum' f = sum . collect f

-- | Collection.maxItem is 'maximum' '.' 'collect'.
--
-- > maxItem (ignoringIndex (* 2)) [1,2,3,4] == 8
maxItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
maxItem f = maximum . collect f

-- | Collection.minItem is 'maximum' '.' 'collect'.
--
-- > minItem (ignoringIndex (* 2)) [1,2,3,4] == 2
minItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
minItem f = minimum . collect f

-- | Variant of 'zipWith' that cycles the shorter input.
--
-- > zipWith_c (+) [1,2] [3,4,5] == [4,6,6]
zipWith_c :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_c f a b =
    let g [] [] _ = []
        g [] b' (_,e) = if e then [] else g a b' (True,e)
        g a' [] (e,_) = if e then [] else g a' b (e,True)
        g (a0 : aN) (b0 : bN) e = f a0 b0 : g aN bN e
    in g a b (False,False)

-- | 'zipWith_c' base variant of 'zip'.
--
-- > zip_c [1,2] [3,4,5] == [(1,3),(2,4),(1,5)]
zip_c :: [a] -> [b] -> [(a,b)]
zip_c = zipWith_c (,)

-- | Variant of 'zipWith3' that cycles the shorter inputs.
--
-- > zipWith3_c (,,) [1] [2,3] [4,5,6] == [(1,2,4),(1,3,5),(1,2,6)]
zipWith3_c :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3_c f p q r =
    let g = map (const ())
        l = [g p,g q,g r]
        f' _ = f
    in zipWith4 f' (extension l) (cycle p) (cycle q) (cycle r)

-- | 'zipWith3_c' based variant of 'zip3'.
--
-- > zip3_c [1] [2,3] [4,5,6] == [(1,2,4),(1,3,5),(1,2,6)]
zip3_c :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3_c = zipWith3_c (\a b c -> (a,b,c))

-- | 'zipWith_c' based variant of applicative '<*>'.
--
-- > zap_c [(+1),negate] [1..6] == [2,-2,4,-4,6,-6]
zap_c :: [a -> b] -> [a] -> [b]
zap_c = zipWith_c (\f e -> f e)

-- * Sequenceable Collection

with_counter :: (a -> (b,a)) -> Int -> a -> [b]
with_counter f =
    let go n i =
            case n of
              0 -> []
              _ -> let (r,i') = f i in r : go n i'
    in go

-- | Arithmetic series (size, start, step)
--
-- > Array.series(5,10,2) == [10,12,14,16,18]
-- > series 5 10 2 == [10,12 .. 18]
--
-- Note that this is quite different from the SimpleNumber.series
-- method, which is equal to 'enumFromThenTo'.
--
-- > 5.series(7,10) == [5,7,9]
-- > enumFromThenTo 5 7 10 == [5,7,9]
series :: (Num a) => Int -> a -> a -> [a]
series n i j =
    case n of
      0 -> []
      _ -> i : series (n - 1) (i + j) j

-- | Geometric series (size, start, grow).
--
-- > Array.geom(5,3,6) == [3,18,108,648,3888]
-- > geom 5 3 6 == [3,18,108,648,3888]
geom :: (Num a) => Int -> a -> a -> [a]
geom n i j =
    case n of
      0 -> []
      _ -> i : geom (n - 1) (i * j) j

-- | Fibonacci series where 'n' number of elements, 'i' is the initial
--   step and 'j' the initial value.
--
-- > Array.fib(5,2,32) == [32,34,66,100,166]
-- > fib 5 2 32 == [32,34,66,100,166]
fib :: (Num a) => Int -> a -> a -> [a]
fib n i j =
    case n of
      0 -> []
      _ -> j : fib (n - 1) j (i + j)

-- | Total variant of 'L.head'.
--
-- > [3,4,5].first == 3
-- > first [3,4,5] == Just 3
-- > first' [3,4,5] == 3
--
-- > [].first == nil
-- > first [] == Nothing
first :: [t] -> Maybe t
first xs =
    case xs of
      [] -> Nothing
      x:_ -> Just x

-- | Synonym for 'L.head'.
first' :: [t] -> t
first' = head

lastM :: [t] -> Maybe t
lastM xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _:xs' -> lastM xs'

-- | Total variant of 'L.last'.
--
-- > (1..5).last == 5
-- > last [1..5] == Just 5
-- > L.last [1..5] == 5
--
-- > [].last == nil
-- > last [] == Nothing
last :: [t] -> Maybe t
last = lastM

-- | Synonym for 'L.last'.
last' :: [t] -> t
last' = L.last

-- | Variant of 'elemIndex' with reversed arguments.
--
-- > [3,4,100,5].indexOf(100) == 2
-- > indexOf [3,4,100,5] 100 == Just 2
indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf = flip elemIndex

indexOf' :: Eq a => [a] -> a -> Int
indexOf' l = fromJust . indexOf l

-- | indexOf
indexOfEqual :: Eq a => [a] -> a -> Maybe Int
indexOfEqual = indexOf

-- | Collection is sorted, index of first greater element.
indexOfGreaterThan :: (Ord a) => a -> [a] -> Maybe Int
indexOfGreaterThan e = detectIndex (ignoringIndex (> e))

-- | Collection is sorted, index of nearest element.
indexIn :: (Ord a,Num a) => a -> [a] -> Int
indexIn e l =
    let f 0 = 0
        f j = let i = j - 1
                  right = l !! j
                  left = l !! i
              in if (e - left) < (right - e) then i else j
    in maybe (size l - 1) f (indexOfGreaterThan e l)

-- | Collection is sorted, linearly interpolated fractional index.
indexInBetween :: (Ord a,Fractional a) => a -> [a] -> a
indexInBetween e l =
    let f 0 = 0
        f j = let i = fromIntegral j
                  a = l !! (j - 1)
                  b = l !! j
                  d = b - a
              in if d == 0 then i else ((e - a) / d) + i - 1
    in maybe (fromIntegral (size l) - 1) f (indexOfGreaterThan e l)

-- | For positive 'n' a synonym for 'take', for negative 'n'
-- a variant on 'L.drop' based on the 'length' of 'l'.
--
-- > [1,2,3,4,5].keep(3) == [1,2,3]
-- > keep 3 [1,2,3,4,5] == [1,2,3]
--
-- > [1,2,3,4,5].keep(-3) == [3,4,5]
-- > keep (-3) [1,2,3,4,5] == [3,4,5]
--
-- > [1,2].keep(-4) == [1,2]
-- > keep (-4) [1,2] == [1,2]
keep :: Int -> [a] -> [a]
keep n l =
    if n < 0
    then L.drop (length l + n) l
    else take n l

-- | For positive 'n' a synonym for 'L.drop', for negative 'n'
-- a variant on 'take' based on the 'length' of 'l'.
--
-- > [1,2,3,4,5].drop(3) == [4,5]
-- > drop 3 [1,2,3,4,5] == [4,5]
--
-- > [1,2,3,4,5].drop(-3) == [1,2]
-- > drop (-3) [1,2,3,4,5] == [1,2]
--
-- > [1,2].drop(-4) == []
-- > drop (-4) [1,2] == []
drop :: Int -> [a] -> [a]
drop n l =
    if n < 0
    then take (length l + n) l
    else L.drop n l

extension :: [[a]] -> [()]
extension x =
    if null x
    then []
    else let x' = filter (not . null) (map tail x)
         in () : extension x'

-- | Variant of 'transpose' that cycles input sequences and extends
-- rather than truncates.
--
-- > [(1..3),(4..5),(6..9)].flop == [[1,4,6],[2,5,7],[3,4,8],[1,5,9]]
-- > flop [[1..3],[4..5],[6..9]] == [[1,4,6],[2,5,7],[3,4,8],[1,5,9]]
--
-- > [[1,2,3],[4,5,6],[7,8]].flop == [[1,4,7],[2,5,8],[3,6,7]]
-- > flop [[1,2,3],[4,5,6],[7,8]] == [[1,4,7],[2,5,8],[3,6,7]]
--
-- The null case at 'flop' is not handled equivalently to SC3
--
-- > [].flop == [[]]
-- > flop [] /= [[]]
-- > flop [] == []
--
-- The 'flop' and 'extendSequences' functions are non-strict and
-- productive.
--
-- > take 4 (flop [[1..3],[4..]]) == [[1,4],[2,5],[3,6],[1,7]]
-- > map (take 4) (extendSequences [[1..3],[4..]]) == [[1,2,3,1],[4,5,6,7]]
flop :: [[a]] -> [[a]]
flop l =
    let l' = map cycle l
    in zipWith (\_ x -> x) (extension l) (transpose l')

-- | Concatenated transposition of cycled subsequences.
--
-- > [[1,2,3],[6],[8,9]].lace(12) == [1,6,8,2,6,9,3,6,8,1,6,9]
-- > lace 12 [[1,2,3],[6],[8,9]] == [1,6,8,2,6,9,3,6,8,1,6,9]
lace :: Int -> [[a]] -> [a]
lace n = take n . concat . transpose . map cycle

-- | Extend sequence by /cycling/.  'wrapExtend' is in terms of 'take'
-- and 'cycle'.
--
-- > [1,2,3,4,5].wrapExtend(9) == [1,2,3,4,5,1,2,3,4]
-- > wrapExtend 9 [1,2,3,4,5] == [1,2,3,4,5,1,2,3,4]
wrapExtend :: Int -> [a] -> [a]
wrapExtend n = take n . cycle

-- | Infinite variant of 'foldExtend'.
cycleFold :: [a] -> [a]
cycleFold = cycle . mirror1

-- | Extend sequence by /folding/ backwards at end.  'foldExtend' is
-- in terms of 'cycleFold', which is in terms of 'mirror1'.
--
-- > [1,2,3,4,5].foldExtend(10)
-- > foldExtend 10 [1,2,3,4,5] == [1,2,3,4,5,4,3,2,1,2]
foldExtend :: Int -> [a] -> [a]
foldExtend n = take n . cycleFold

-- | Extend sequence by repeating last element.
--
-- > [1,2,3,4,5].clipExtend(9) == [1,2,3,4,5,5,5,5,5]
-- > clipExtend 9 [1,2,3,4,5] == [1,2,3,4,5,5,5,5,5]
clipExtend :: Int -> [a] -> [a]
clipExtend n = take n . cycleClip

cycleClip :: [a] -> [a]
cycleClip l =
    case lastM l of
      Nothing -> []
      Just e -> l ++ repeat e

extendSequences :: [[a]] -> [[a]]
extendSequences l =
    let f = zipWith (\_ x -> x) (extension l) . cycle
    in map f l

separateAt :: (a -> a -> Bool) -> [a] -> ([a],[a])
separateAt f xs =
    case xs of
      (x1:x2:xs') ->
          if f x1 x2
          then ([x1],x2:xs')
          else let g e (l,r) = (e:l,r)
               in x1 `g` separateAt f (x2:xs')
      _ -> (xs,[])

-- | Separates by applying the predicate 'f' to each adjacent pair of
-- elements at 'l'. If the predicate is 'True', then a separation is
-- made between the elements.
--
-- > [3,2,1,2,3,2].separate({|a,b| a<b}) == [[3,2,1],[2],[3,2]]
-- > separate (<) [3,2,1,2,3,2] == [[3,2,1],[2],[3,2]]
--
-- > [1,2,3,5,6,8].separate({|a,b| (b - a) > 1}) == [[1,2,3],[5,6],[8]]
-- > separate (\a b -> (b - a) > 1) [1,2,3,5,6,8] == [[1,2,3],[5,6],[8]]
separate :: (a -> a -> Bool) -> [a] -> [[a]]
separate f l =
    let (e,r) = separateAt f l
    in if null r then [e] else e : separate f r

-- | Synonym for 'Data.List.Split.splitEvery'.
--
-- > [1,2,3,4,5,6,7,8].clump(3) == [[1,2,3],[4,5,6],[7,8]]
-- > clump 3 [1,2,3,4,5,6,7,8] == [[1,2,3],[4,5,6],[7,8]]
clump :: Int -> [a] -> [[a]]
clump = splitEvery

-- | Synonym for 'Data.List.Split.splitPlaces'.
--
-- > [1,2,3,4,5,6,7,8].clumps([1,2]) == [[1],[2,3],[4],[5,6],[7],[8]]
-- > clumps [1,2] [1,2,3,4,5,6,7,8] == [[1],[2,3],[4],[5,6],[7],[8]]
clumps :: [Int] -> [a] -> [[a]]
clumps m s =
    let f [] _ = undefined
        f (n:ns) l = let (e,r) = splitAt n l
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

-- | Rotate n places to the left
--
-- > rotateLeft 3 [1..7] == [4,5,6,7,1,2,3]
rotateLeft :: Int -> [a] -> [a]
rotateLeft n p =
    let (b,a) = splitAt n p
    in a ++ b

-- | Rotate n places to the right
--
-- > rotateRight 3 [1..7] == [5,6,7,1,2,3,4]
rotateRight :: Int -> [a] -> [a]
rotateRight n p =
    let k = length p
        (b,a) = splitAt (k - n) p
    in a ++ b

-- | Ensure sum of elements is one.
normalizeSum :: (Fractional a) => [a] -> [a]
normalizeSum l =
    let n = sum l
    in map (/ n) l

-- | Identity window function with subsequences of length 'w' and
-- stride of 'n'.
--
-- > [1,2,3,4,5,6].slide(3,1)
-- > slide 3 1 [1,2,3,4,5,6] == [1,2,3,2,3,4,3,4,5,4,5,6]
--
-- > [1,2,3,4,5,6].slide(3,2)
-- > slide 3 2 [1,2,3,4,5,6] == [1,2,3,3,4,5]
--
-- > [1,2,3,4,5,6].slide(4,2)
-- > slide 4 2 [1,2,3,4,5,6] == [1,2,3,4,3,4,5,6]
slide :: Int -> Int -> [a] -> [a]
slide w n l =
    let k = length l
    in concat (map (\i -> take w (L.drop i l)) [0,n .. k-w])

-- | Concatentate with 'tail' of 'reverse' to make a palindrome.
--
-- > [1,2,3,4].mirror == [1,2,3,4,3,2,1]
-- > mirror [1,2,3,4] == [1,2,3,4,3,2,1]
mirror :: [a] -> [a]
mirror l = l ++ (tail (reverse l))

-- | As 'mirror' but with last element removed.
--
-- > [1,2,3,4].mirror1 == [1,2,3,4,3,2]
-- > mirror1 [1,2,3,4] == [1,2,3,4,3,2]
mirror1 :: [a] -> [a]
mirror1 l =
    case l of
      [] -> []
      [e] -> [e]
      _ -> l ++ tail (reverse (tail l))

-- | Concatenate with 'reverse' to make a palindrome, as 'mirror'
-- does, but with the center element duplicated.
--
-- > [1,2,3,4].mirror2 == [1,2,3,4,4,3,2,1]
-- > mirror2 [1,2,3,4] == [1,2,3,4,4,3,2,1]
mirror2 :: [a] -> [a]
mirror2 l = l ++ (reverse l)

-- | Repeated each element 'n' times.
--
-- > [1,2,3].stutter(2) == [1,1,2,2,3,3]
-- > stutter 2 [1,2,3] == [1,1,2,2,3,3]
stutter :: Int -> [a] -> [a]
stutter n = concatMap (replicate n)

-- | 'rotate' is in terms of 'rotateLeft' and 'rotateRight', where
--    negative 'n' rotates left and positive 'n' rotates right.
--
-- > (1..5).rotate(1) == [5,1,2,3,4]
-- > rotate 1 [1..5] == [5,1,2,3,4]
--
-- > (1..5).rotate(-1) == [2,3,4,5,1]
-- > rotate (-1) [1..5] == [2,3,4,5,1]
--
-- > (1..5).rotate(3) == [3,4,5,1,2]
-- > rotate 3 [1..5] == [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate n = if n < 0 then rotateLeft n else rotateRight n
