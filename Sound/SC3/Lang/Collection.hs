module Sound.SC3.Lang.Collection where

import Data.List.Split {- split -}
import Data.List as L
import Data.Maybe

-- * Collection

fill :: Int -> (Int -> a) -> [a]
fill n f = map f [0 .. n - 1]

size :: [a] -> Int
size = length

isEmpty :: [a] -> Bool
isEmpty = null

ignoringIndex :: (a -> b) -> a -> Int -> b
ignoringIndex f e _ = f e

collect :: (a -> Int -> b) -> [a] -> [b]
collect f l = zipWith f l [0..]

select :: (a -> Int -> Bool) -> [a] -> [a]
select f l = map fst (filter (uncurry f) (zip l [0..]))

reject :: (a -> Int -> Bool) -> [a] -> [a]
reject f l = map fst (filter (not . uncurry f) (zip l [0..]))

detect :: (a -> Int -> Bool) -> [a] -> Maybe a
detect f l = maybe Nothing (Just . fst) (find (uncurry f) (zip l [0..]))

detectIndex :: (a -> Int -> Bool) -> [a] -> Maybe Int
detectIndex f l = maybe Nothing (Just . snd) (find (uncurry f) (zip l [0..]))

inject :: a -> (a -> b -> a) -> [b] -> a
inject i f = foldl f i

any' :: (a -> Int -> Bool) -> [a] -> Bool
any' f = isJust . detect f

every :: (a -> Int -> Bool) -> [a] -> Bool
every f = let g e = not . f e
          in not . any' g

count :: (a -> Int -> Bool) -> [a] -> Int
count f = length . select f

occurencesOf :: (Eq a) => a -> [a] -> Int
occurencesOf k = count (\e _ -> e == k)

sum' :: (Num a) => (b -> Int -> a) -> [b] -> a
sum' f = sum . collect f

maxItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
maxItem f = maximum . collect f

minItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
minItem f = minimum . collect f

-- | Variant that cycles the shorter input.
zipWith_c :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_c f a b =
    let g [] [] _ = []
        g [] b' (_,e) = if e then [] else g a b' (True,e)
        g a' [] (e,_) = if e then [] else g a' b (e,True)
        g (a0 : aN) (b0 : bN) e = f a0 b0 : g aN bN e
    in g a b (False,False)

zip_c :: [a] -> [b] -> [(a,b)]
zip_c = zipWith_c (,)

zipWith3_c :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3_c f p q r =
    let g = map (const ())
        l = [g p,g q,g r]
        f' _ = f
    in zipWith4 f' (extension l) (cycle p) (cycle q) (cycle r)

zip3_c :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3_c = zipWith3_c (\a b c -> (a,b,c))

{-
zip3_c [1..2] [3..5] [6..9]
-}

zap_c :: [a -> b] -> [a] -> [b]
zap_c = zipWith_c (\f e -> f e)

{-
zap_c [(+1)] [1..10]
-}

-- * Sequenceable Collection

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
      _ -> i : geom (n - 1) (i * j) j

-- | Fibonacci series (size, initial step, start)
fib :: (Num a) => Int -> a -> a -> [a]
fib n i j =
    case n of
      0 -> []
      _ -> j : fib (n - 1) j (i + j)

-- | The first element.
first :: [t] -> Maybe t
first xs =
    case xs of
      [] -> Nothing
      x:_ -> Just x

first' :: [t] -> t
first' = head

lastM :: [t] -> Maybe t
lastM xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _:xs' -> lastM xs'

-- | The last element.
last :: [t] -> Maybe t
last = lastM

last' :: [t] -> t
last' = L.last

-- | flip elemIndex
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

keep :: Int -> [a] -> [a]
keep n l =
    if n < 0
    then L.drop (length l + n) l
    else take n l

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

flop :: [[a]] -> [[a]]
flop l =
    let l' = map cycle l
    in zipWith (\_ x -> x) (extension l) (transpose l')

lace :: Int -> [[a]] -> [a]
lace n = take n . concat . transpose . map cycle

wrapExtend :: Int -> [a] -> [a]
wrapExtend n = take n . cycle

cycleFold :: [a] -> [a]
cycleFold = cycle . mirror1

foldExtend :: Int -> [a] -> [a]
foldExtend n = take n . cycleFold

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

separate :: (a -> a -> Bool) -> [a] -> [[a]]
separate f l =
    let (e,r) = separateAt f l
    in if null r then [e] else e : separate f r

clump :: Int -> [a] -> [[a]]
clump = splitEvery

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

-- | Rotate n places to the left (ie. rotateLeft 1 [1,2,3] is [2,3,1]).
rotateLeft :: Int -> [a] -> [a]
rotateLeft n p =
    let (b,a) = splitAt n p
    in a ++ b

-- | Rotate n places to the right (ie. rotateRight 1 [1,2,3] is [3,1,2]).
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

slide :: Int -> Int -> [a] -> [a]
slide w n l =
    let k = length l
    in concat (map (\i -> take w (L.drop i l)) [0,n .. k-w])

mirror :: [a] -> [a]
mirror l = l ++ (tail (reverse l))

mirror1 :: [a] -> [a]
mirror1 l =
    case l of
      [] -> []
      [e] -> [e]
      _ -> l ++ tail (reverse (tail l))

mirror2 :: [a] -> [a]
mirror2 l = l ++ (reverse l)

stutter :: Int -> [a] -> [a]
stutter n = concatMap (replicate n)

rotate :: Int -> [a] -> [a]
rotate n = if n < 0 then rotateLeft n else rotateRight n
