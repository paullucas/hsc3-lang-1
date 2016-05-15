-- | In cases where a method takes arguments, these precede the
-- collection argument in the haskell variant, so that @c.m(i,j)@
-- becomes @m i j c@.
module Sound.SC3.Lang.Collection where

import Data.List.Split {- split -}
import Data.List as L {- base -}
import Data.Maybe {- base -}

import qualified Sound.SC3 as S {- hsc3 -}

import Sound.SC3.Lang.Core {- hsc3-lang -}

-- * Collection

-- | @Collection.*fill@ is 'map' over indices to /n/.
--
-- > fill 4 (* 2) == [0,2,4,6]
fill :: (Enum n,Num n) => n -> (n -> a) -> [a]
fill n f = map f [0 .. n - 1]

-- | @Collection.size@ is 'length'.
--
-- > size [1,2,3,4] == 4
size :: Integral n => [a] -> n
size = genericLength

-- | @Collection.isEmpty@ is 'null'.
--
-- > isEmpty [] == True
isEmpty :: [a] -> Bool
isEmpty = null

-- | Function equal to 'const' of /f/ of /e/.
--
-- > select (ignoringIndex even) [1,2,3,4] == [2,4]
ignoringIndex :: (a -> b) -> a -> z -> b
ignoringIndex f e = const (f e)

-- | @Collection.collect@ is 'map' with element indices.
--
-- > collect (\i _ -> i + 10) [1,2,3,4] == [11,12,13,14]
-- > collect (\_ j -> j + 11) [1,2,3,4] == [11,12,13,14]
collect :: Integral i => (a -> i -> b) -> [a] -> [b]
collect f l = zipWith f l [0..]

-- | @Collection.select@ is 'filter' with element indices.
--
-- > select (\i _ -> even i) [1,2,3,4] == [2,4]
-- > select (\_ j -> even j) [1,2,3,4] == [1,3]
select :: Integral i => (a -> i -> Bool) -> [a] -> [a]
select f l = map fst (filter (uncurry f) (zip l [0..]))

-- | @Collection.reject@ is negated 'filter' with element indices.
--
-- > reject (\i _ -> even i) [1,2,3,4] == [1,3]
-- > reject (\_ j -> even j) [1,2,3,4] == [2,4]
reject :: Integral i => (a -> i -> Bool) -> [a] -> [a]
reject f l = map fst (filter (not . uncurry f) (zip l [0..]))

-- | @Collection.detect@ is 'first' '.' 'select'.
--
-- > detect (\i _ -> even i) [1,2,3,4] == Just 2
detect :: Integral i => (a -> i -> Bool) -> [a] -> Maybe a
detect f l = fmap fst (find (uncurry f) (zip l [0..]))

-- | @Collection.detectIndex@ is the index locating variant of 'detect'.
--
-- > detectIndex (\i _ -> even i) [1,2,3,4] == Just 1
detectIndex :: Integral i => (a -> i -> Bool) -> [a] -> Maybe i
detectIndex f l = fmap snd (find (uncurry f) (zip l [0..]))

-- | @Collection.inject@ is a variant on 'foldl'.
--
-- > inject 0 (+) [1..5] == 15
-- > inject 1 (*) [1..5] == 120
inject :: a -> (a -> b -> a) -> [b] -> a
inject i f = foldl f i

-- | @Collection.any@ is 'True' if 'detect' is not 'Nothing'.
--
-- > any' (\i _ -> even i) [1,2,3,4] == True
any' :: Integral i => (a -> i -> Bool) -> [a] -> Bool
any' = isJust .: detect

-- | @Collection.every@ is 'True' if /f/ applies at all elements.
--
-- > every (\i _ -> even i) [1,2,3,4] == False
every :: Integral i => (a -> i -> Bool) -> [a] -> Bool
every f =
    let g = not .: f
    in not . any' g

-- | @Collection.count@ is 'length' of 'select'.
--
-- > count (\i _ -> even i) [1,2,3,4] == 2
count :: Integral i => (a -> i -> Bool) -> [a] -> i
count = genericLength .: select

-- | @Collection.occurencesOf@ is an '==' variant of 'count'.
--
-- > occurencesOf 2 [1,2,3,4] == 1
-- > occurencesOf 't' "test" == 2
occurencesOf :: (Integral i,Eq a) => a -> [a] -> i
occurencesOf k = count (\e _ -> e == k)

-- | @Collection.sum@ is 'sum' of 'collect'.
--
-- > sum' (ignoringIndex (* 2)) [1,2,3,4] == 20
sum' :: (Num a,Integral i) => (b -> i -> a) -> [b] -> a
sum' = sum .: collect

-- | @Collection.maxItem@ is 'maximum' of 'collect'.
--
-- > maxItem (ignoringIndex (* 2)) [1,2,3,4] == 8
maxItem :: (Ord b,Integral i) => (a -> i -> b) -> [a] -> b
maxItem = maximum .: collect

-- | @Collection.minItem@ is 'maximum' '.' 'collect'.
--
-- > minItem (ignoringIndex (* 2)) [1,2,3,4] == 2
minItem :: (Integral i,Ord b) => (a -> i -> b) -> [a] -> b
minItem = minimum .: collect

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

-- | 'zipWith_c' variant of 'zip'.
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

-- | @SequenceableCollection.*series@ is an arithmetic series with
-- arguments /size/, /start/ and /step/.
--
-- > > Array.series(5,10,2) == [10,12,14,16,18]
-- > series 5 10 2 == [10,12 .. 18]
--
-- Note that this is quite different from the SimpleNumber.series
-- method, which is equal to 'enumFromThenTo'.
--
-- > > 5.series(7,10) == [5,7,9]
-- > enumFromThenTo 5 7 10 == [5,7,9]
series :: (Num a,Integral i) => i -> a -> a -> [a]
series n i j =
    case n of
      0 -> []
      _ -> i : series (n - 1) (i + j) j

-- | @SequenceableCollection.*geom@ is a geometric series with arguments
-- /size/, /start/ and /grow/.
--
-- > > Array.geom(5,3,6) == [3,18,108,648,3888]
-- > geom 5 3 6 == [3,18,108,648,3888]
geom :: (Integral i,Num a) => i -> a -> a -> [a]
geom n i j =
    case n of
      0 -> []
      _ -> i : geom (n - 1) (i * j) j

-- | @SequenceableCollection.*fib@ is the Fibonacci series where /n/
-- is number of elements, /i/ is the initial step and /j/ the initial
-- value.
--
-- > > Array.fib(5,2,32) == [32,34,66,100,166]
-- > fib 5 2 32 == [32,34,66,100,166]
fib :: (Integral i,Num a) => i -> a -> a -> [a]
fib n i j =
    case n of
      0 -> []
      _ -> j : fib (n - 1) j (i + j)

-- | @SequenceableCollection.first@ is a total variant of 'head'.
--
-- > > [3,4,5].first == 3
-- > first [3,4,5] == Just 3
-- > first' [3,4,5] == 3
--
-- > > [].first == nil
-- > first [] == Nothing
first :: [t] -> Maybe t
first xs =
    case xs of
      [] -> Nothing
      x:_ -> Just x

-- | Synonym for 'head'.
first' :: [t] -> t
first' = head

-- | Total variant of 'L.last'.
--
-- > > (1..5).last == 5
-- > lastM [1..5] == Just 5
-- > L.last [1..5] == 5
--
-- > > [].last == nil
-- > lastM [] == Nothing
lastM :: [t] -> Maybe t
lastM xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _:xs' -> lastM xs'

-- | @SequenceableCollection.last@ is a synonym for 'lastM'.
last :: [t] -> Maybe t
last = lastM

-- | Synonym for 'L.last'.
last' :: [t] -> t
last' = L.last

-- | @SequenceableCollection.indexOf@ is a variant of 'elemIndex' with
-- reversed arguments.
--
-- > > [3,4,100,5].indexOf(100) == 2
-- > indexOf [3,4,100,5] 100 == Just 2
indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf = flip elemIndex

-- | 'fromJust' variant of 'indexOf'.
indexOf' :: Eq a => [a] -> a -> Int
indexOf' = fromJust .: indexOf

-- | @SequenceableCollection.indexOfEqual@ is just 'indexOf'.
indexOfEqual :: Eq a => [a] -> a -> Maybe Int
indexOfEqual = indexOf

-- | @SequenceableCollection.indexOfGreaterThan@ is the index of the
-- first greater element.
--
-- > indexOfGreaterThan 70 [10,5,77,55,12,123] == Just 2
indexOfGreaterThan :: (Ord a) => a -> [a] -> Maybe Int
indexOfGreaterThan e = detectIndex (ignoringIndex (> e))

-- | @SequenceableCollection.indexIn@ is the index of nearest element.
--
-- > indexIn 5.2 [2,3,5,6] == 2
indexIn :: (Ord a,Num a) => a -> [a] -> Int
indexIn e l =
    let f 0 = 0
        f j = let i = j - 1
                  right = l !! j
                  left = l !! i
              in if (e - left) < (right - e) then i else j
    in maybe (size l - 1) f (indexOfGreaterThan e l)

-- | @SequenceableCollection.indexInBetween@ is the linearly
-- interpolated fractional index.  Collection must be sorted. The
-- inverse operation is 'blendAt'.
--
-- > > [2,3,5,6].indexInBetween(5.2) == 2.2
-- > indexInBetween 5.2 [2,3,5,6] == 2.2
indexInBetween :: (Ord a,Fractional a) => a -> [a] -> a
indexInBetween e l =
    let f 0 = 0
        f j = let i = fromIntegral j
                  a = l !! (j - 1)
                  b = l !! j
                  d = b - a
              in if d == 0 then i else ((e - a) / d) + i - 1
    in maybe (fromInteger (size l) - 1) f (indexOfGreaterThan e l)

-- | @SequenceableCollection.keep@ is, for positive /n/ a synonym for
-- 'take', and for negative /n/ a variant on 'drop' based on the
-- 'length' of /l/.
--
-- > > [1,2,3,4,5].keep(3) == [1,2,3]
-- > keep 3 [1,2,3,4,5] == [1,2,3]
--
-- > > [1,2,3,4,5].keep(-3) == [3,4,5]
-- > keep (-3) [1,2,3,4,5] == [3,4,5]
--
-- > > [1,2].keep(-4) == [1,2]
-- > keep (-4) [1,2] == [1,2]
keep :: Integral i => i -> [a] -> [a]
keep n l =
    if n < 0
    then genericDrop (genericLength l + n) l
    else genericTake n l

-- | @SequenceableCollection.drop@ is, for positive /n/ a synonym for
-- 'L.drop', for negative /n/ a variant on 'L.take' based on the
-- 'L.length' of /l/.
--
-- > > [1,2,3,4,5].drop(3) == [4,5]
-- > L.drop 3 [1,2,3,4,5] == [4,5]
--
-- > > [1,2,3,4,5].drop(-3) == [1,2]
-- > Sound.SC3.Lang.Collection.drop (-3) [1,2,3,4,5] == [1,2]
--
-- > > [1,2].drop(-4) == []
-- > Sound.SC3.Lang.Collection.drop (-4) [1,2] == []
drop :: Integral i => i -> [a] -> [a]
drop n l =
    if n < 0
    then genericTake (genericLength l + n) l
    else genericDrop n l

-- | Function to calculate a list equal in length to the longest input
-- list, therefore being productive over infinite lists.
--
-- > extension [[1],[2,3],[4,5,6]] == [(),(),()]
-- > take 3 (extension [[1],[2..]]) == [(),(),()]
extension :: [[a]] -> [()]
extension x =
    if null x
    then []
    else let x' = filter (not . null) (map tail x)
         in () : extension x'

-- | @SequenceableCollection.flop@ is a variant of 'transpose' that
-- cycles input sequences and extends rather than truncates.
--
-- > > [(1..3),(4..5),(6..9)].flop == [[1,4,6],[2,5,7],[3,4,8],[1,5,9]]
-- > flop [[1..3],[4..5],[6..9]] == [[1,4,6],[2,5,7],[3,4,8],[1,5,9]]
--
-- > > [[1,2,3],[4,5,6],[7,8]].flop == [[1,4,7],[2,5,8],[3,6,7]]
-- > flop [[1,2,3],[4,5,6],[7,8]] == [[1,4,7],[2,5,8],[3,6,7]]
--
-- The null case at 'flop' is not handled equivalently to SC3
--
-- > > [].flop == [[]]
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

-- | @SequenceableCollection.integrate@ is the incremental sum of
-- elements.
--
-- > > [3,4,1,1].integrate == [3,7,8,9]
-- > integrate [3,4,1,1] == [3,7,8,9]
integrate :: (Num a) => [a] -> [a]
integrate = scanl1 (+)

-- | @SequenceableCollection.differentiate@ is the pairwise difference
-- between elements, with an implicit @0@ at the start.
--
-- > > [3,4,1,1].differentiate == [3,1,-3,0]
-- > differentiate [3,4,1,1] == [3,1,-3,0]
--
-- > > [0,3,1].differentiate == [0,3,-2]
-- > differentiate [0,3,1] == [0,3,-2]
differentiate :: (Num a) => [a] -> [a]
differentiate l = zipWith (-) l (0:l)

-- | Variant of 'separate' that performs initial separation.
separateAt :: (a -> a -> Bool) -> [a] -> ([a],[a])
separateAt f xs =
    case xs of
      (x1:x2:xs') ->
          if f x1 x2
          then ([x1],x2:xs')
          else let g e (l,r) = (e:l,r)
               in x1 `g` separateAt f (x2:xs')
      _ -> (xs,[])

-- | @SequenceableCollection.separate@ applies the predicate 'f' to
-- each adjacent pair of elements at /l/. If the predicate is 'True',
-- then a separation is made between the elements.
--
-- > > [3,2,1,2,3,2].separate({|a,b| a<b}) == [[3,2,1],[2],[3,2]]
-- > separate (<) [3,2,1,2,3,2] == [[3,2,1],[2],[3,2]]
--
-- > > [1,2,3,5,6,8].separate({|a,b| (b - a) > 1}) == [[1,2,3],[5,6],[8]]
-- > separate (\a b -> (b - a) > 1) [1,2,3,5,6,8] == [[1,2,3],[5,6],[8]]
separate :: (a -> a -> Bool) -> [a] -> [[a]]
separate f l =
    let (e,r) = separateAt f l
    in if null r then [e] else e : separate f r

-- | @SequenceableCollection.clump@ is a synonym for
-- 'Data.List.Split.chunksOf'.
--
-- > > [1,2,3,4,5,6,7,8].clump(3) == [[1,2,3],[4,5,6],[7,8]]
-- > clump 3 [1,2,3,4,5,6,7,8] == [[1,2,3],[4,5,6],[7,8]]
clump :: Int -> [a] -> [[a]]
clump = chunksOf

-- | @SequenceableCollection.clumps@ is a synonym for
-- 'Data.List.Split.splitPlaces'.
--
-- > > [1,2,3,4,5,6,7,8].clumps([1,2]) == [[1],[2,3],[4],[5,6],[7],[8]]
-- > clumps [1,2] [1,2,3,4,5,6,7,8] == [[1],[2,3],[4],[5,6],[7],[8]]
clumps :: [Int] -> [a] -> [[a]]
clumps m s =
    let f [] _ = undefined
        f (n:ns) l = let (e,r) = splitAt n l
                     in if null r then [e] else e : clumps ns r
    in case m of
         [] -> []
         _ -> f (cycle m) s

-- * List and Array

-- | @List.lace@ is a concatenated transposition of cycled
-- subsequences.
--
-- > > [[1,2,3],[6],[8,9]].lace(12) == [1,6,8,2,6,9,3,6,8,1,6,9]
-- > lace 12 [[1,2,3],[6],[8,9]] == [1,6,8,2,6,9,3,6,8,1,6,9]
lace :: Integral i => i -> [[a]] -> [a]
lace n = genericTake n . concat . transpose . map cycle

-- | @List.wrapExtend@ extends a sequence by
-- /cycling/.  'wrapExtend' is in terms of 'take' and 'cycle'.
--
-- > > [1,2,3,4,5].wrapExtend(9) == [1,2,3,4,5,1,2,3,4]
-- > wrapExtend 9 [1,2,3,4,5] == [1,2,3,4,5,1,2,3,4]
wrapExtend :: Integral i => i -> [a] -> [a]
wrapExtend n = genericTake n . cycle

-- | Infinite variant of 'foldExtend'.
cycleFold :: [a] -> [a]
cycleFold = cycle . mirror1

-- | @List.foldExtend@ extends sequence by /folding/ backwards at end.
-- 'foldExtend' is in terms of 'cycleFold', which is in terms of
-- 'mirror1'.
--
-- > > [1,2,3,4,5].foldExtend(10)
-- > foldExtend 10 [1,2,3,4,5] == [1,2,3,4,5,4,3,2,1,2]
foldExtend :: Integral i => i -> [a] -> [a]
foldExtend n = genericTake n . cycleFold

-- | @Array.clipExtend@ extends sequence by repeating last element.
--
-- > > [1,2,3,4,5].clipExtend(9) == [1,2,3,4,5,5,5,5,5]
-- > clipExtend 9 [1,2,3,4,5] == [1,2,3,4,5,5,5,5,5]
clipExtend :: Integral i => i -> [a] -> [a]
clipExtend n = genericTake n . cycleClip

-- | Infinite variant of 'clipExtend'.
cycleClip :: [a] -> [a]
cycleClip l =
    case lastM l of
      Nothing -> []
      Just e -> l ++ repeat e

-- | Cycle input sequences to 'extension' of input.
--
-- > extendSequences [[1],[2,3],[4,5,6]] == [[1,1,1],[2,3,2],[4,5,6]]
extendSequences :: [[a]] -> [[a]]
extendSequences l =
    let f = zipWith (\_ x -> x) (extension l) . cycle
    in map f l

-- | @ArrayedCollection.asRandomTable@ returns an integral table that
-- can be used to generate random numbers with a specified
-- distribution.
--
-- > > [1,0,1,0,1,0,1].asRandomTable(256).plot
-- > > ((0..100) ++ (100..50) / 100).asRandomTable.plot
--
-- > import Sound.SC3.Plot
-- > plotTable [asRandomTable 256 [1,0,1,0,1,0,1]]
-- > plotTable [asRandomTable 256 (map (/ 100) ([0..100] ++ [100,99..50]))]
asRandomTable :: (Enum a, RealFrac a) => Int -> [a] -> [a]
asRandomTable n c =
    let n' = fromIntegral n
        a = integrate (S.resamp1 n c)
        b = S.normalize 0 (n' - 1) a
    in map (\i -> indexInBetween i b / n') [0 .. n' - 1]

-- | Non-catenating variant of 'slide'.
slide' :: Integral i => i -> i -> [a] -> [[a]]
slide' w n l = map (\i -> genericTake w (genericDrop i l)) [0,n .. genericLength l - w]

-- | @List.slide@ is an identity window function with subsequences of
-- length /w/ and stride of /n/.
--
-- > > [1,2,3,4,5,6].slide(3,1)
-- > slide 3 1 [1,2,3,4,5,6] == [1,2,3,2,3,4,3,4,5,4,5,6]
--
-- > > [1,2,3,4,5,6].slide(3,2)
-- > slide 3 2 [1,2,3,4,5,6] == [1,2,3,3,4,5]
--
-- > > [1,2,3,4,5,6].slide(4,2)
-- > slide 4 2 [1,2,3,4,5,6] == [1,2,3,4,3,4,5,6]
slide :: Integral i => i -> i -> [a] -> [a]
slide w n = concat . slide' w n

-- | @List.mirror@ concatentates with 'tail' of 'reverse' to make a
-- palindrome.
--
-- > > [1,2,3,4].mirror == [1,2,3,4,3,2,1]
-- > mirror [1,2,3,4] == [1,2,3,4,3,2,1]
mirror :: [a] -> [a]
mirror l = l ++ tail (reverse l)

-- | @List.mirror1@ is as 'mirror' but with last element removed.
--
-- > > [1,2,3,4].mirror1 == [1,2,3,4,3,2]
-- > mirror1 [1,2,3,4] == [1,2,3,4,3,2]
mirror1 :: [a] -> [a]
mirror1 l =
    case l of
      [] -> []
      [e] -> [e]
      _ -> l ++ tail (reverse (tail l))

-- | @List.mirror2@ concatenate with 'reverse' to make a palindrome,
-- as 'mirror' does, but with the center element duplicated.
--
-- > > [1,2,3,4].mirror2 == [1,2,3,4,4,3,2,1]
-- > mirror2 [1,2,3,4] == [1,2,3,4,4,3,2,1]
mirror2 :: [a] -> [a]
mirror2 l = l ++ reverse l

-- | @List.stutter@ repeats each element /n/ times.
--
-- > > [1,2,3].stutter(2) == [1,1,2,2,3,3]
-- > stutter 2 [1,2,3] == [1,1,2,2,3,3]
stutter :: Integral i => i -> [a] -> [a]
stutter n = concatMap (genericReplicate n)

-- | Rotate /n/ places to the left.
--
-- > rotateLeft 1 [1..5] == [2,3,4,5,1]
-- > rotateLeft 3 [1..7] == [4,5,6,7,1,2,3]
rotateLeft :: Integral i => i -> [a] -> [a]
rotateLeft n p =
    let (b,a) = genericSplitAt n p
    in a ++ b

-- | Rotate /n/ places to the right.
--
-- > rotateRight 1 [1..5] == [5,1,2,3,4]
-- > rotateRight 3 [1..7] == [5,6,7,1,2,3,4]
rotateRight :: Integral i => i -> [a] -> [a]
rotateRight n p =
    let k = genericLength p
        (b,a) = genericSplitAt (k - n) p
    in a ++ b

-- | @Array.rotate@ is in terms of 'rotateLeft' and 'rotateRight',
-- where negative /n/ rotates left and positive /n/ rotates right.
--
-- > > (1..5).rotate(1) == [5,1,2,3,4]
-- > rotate 1 [1..5] == [5,1,2,3,4]
--
-- > > (1..5).rotate(-1) == [2,3,4,5,1]
-- > rotate (-1) [1..5] == [2,3,4,5,1]
--
-- > > (1..5).rotate(3) == [3,4,5,1,2]
-- > rotate 3 [1..5] == [3,4,5,1,2]
rotate :: Integral i => i -> [a] -> [a]
rotate n = if n < 0 then rotateLeft (- n) else rotateRight n

-- | @ArrayedCollection.windex@ takes a list of probabilities, which
-- should sum to /n/, and returns the an index value given a (0,/n/)
-- input.
--
-- > mapMaybe (windex [0.1,0.3,0.6]) [0,0.1 .. 0.4] == [0,1,1,1,2]
windex :: (Ord a,Num a) => [a] -> a -> Maybe Int
windex w n = findIndex (n <) (integrate w)

-- * Required

-- * Variants

-- | Variant where all inputs are lists and the result is not
-- catentated.  Does not generate partial windows.
--
-- > let r = ["abc","bc","def","ef","ghi","hi"]
-- > in slide1 (cycle [3,2]) (cycle [1,2]) ['a'..'i'] == r
--
-- > let r = ["abc","bc","bcd","cd","cde","de"]
-- > in slide1 (cycle [3,2]) (cycle [1,0]) ['a'..'e'] == r
slide1 :: Integral i => [i] -> [i] -> [a] -> [[a]]
slide1 w n l =
    case (w,n,l) of
      (w0:w',n0:n',_) -> case genericTakeMaybe w0 l of
                           Nothing -> []
                           Just r -> let l' = genericDrop n0 l
                                     in r : slide1 w' n' l'
      _ -> []

-- | 'concat' of 'slide1'.
slide2 :: Integral i => [i] -> [i] -> [a] -> [a]
slide2 = concat .:: slide1

-- | Variant where stutter input is a list and the result is not
-- catentated.
--
-- > stutter1 [2,1,2] [1,2,3] == [[1,1],[2],[3,3]]
stutter1 :: Integral i => [i] -> [a] -> [[a]]
stutter1 = zipWith genericReplicate

-- | 'concat' of 'stutter1'.
stutter2 :: Integral i => [i] -> [a] -> [a]
stutter2 = concat .: stutter1
