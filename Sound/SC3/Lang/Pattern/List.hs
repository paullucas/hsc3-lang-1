-- | List variants of @SC3@ pattern functions.
module Sound.SC3.Lang.Pattern.List where

import qualified Data.Map as M {- containers -}
import Data.List {- base -}
import System.Random {- random -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

import qualified Sound.SC3.Lang.Collection as Collection
import qualified Sound.SC3.Lang.Core as Core
import qualified Sound.SC3.Lang.Pattern.Stream as Stream

-- * Data.Bool variants

-- | '>' @0@.  Values greater than zero are 'True' and zero and
-- negative values are 'False'.
bool :: (Ord n,Num n) => n -> Bool
bool = (> 0)

-- * Data.Functor variants

-- | 'fmap' of 'bool'.
--
-- > fbool [2,1,0,-1] == [True,True,False,False]
fbool :: (Ord a,Num a,Functor f) => f a -> f Bool
fbool = fmap (> 0)

-- | SC3 pattern to fold values to lie within range (as opposed to
-- wrap and clip).  This is /not/ related to "Data.Foldable".
--
-- > ffold [10,11,12,-6,-7,-8] (-7) 11 == [10,11,10,-6,-7,-6]
--
-- The underlying primitive is the 'SC3.fold_' function.
--
-- > let f n = SC3.fold_ n (-7) 11
-- > map f [10,11,12,-6,-7,-8] == [10,11,10,-6,-7,-6]
ffold :: (Functor f,Num a,Ord a) => f a -> a -> a -> f a
ffold p i j = fmap (\n -> SC3.fold_ n i j) p

-- | SC3 pattern to constrain the range of output values by wrapping,
-- the primitive is 'SC3.generic_wrap'.
--
-- > let p = fmap round (fwrap (geom 200 1.2 10) 200 1000)
-- > p == [200,240,288,346,415,498,597,717,860,231]
fwrap :: (Functor f,Ord a,Num a) => f a -> a -> a -> f a
fwrap xs l r = fmap (SC3.generic_wrap (l,r)) xs

-- * Non-SC3 Patterns

-- | Count the number of `False` values following each `True` value.
--
-- > countpost (map bool [1,0,1,0,0,0,1,1]) == [1,3,0,0]
countpost :: [Bool] -> [Int]
countpost =
    let f i p = if null p
                then [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if not x then f (i + 1) xs else r
    in tail . f 0

-- | Count the number of `False` values preceding each `True` value.
--
-- > countpre (fbool [0,0,1,0,0,0,1,1]) == [2,3,0]
countpre :: [Bool] -> [Int]
countpre =
    let f i p = if null p
                then if i == 0 then [] else [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if x then r else f (i + 1) xs
    in f 0

-- | Sample and hold initial value.
--
-- > hold [] == []
-- > hold [1..5] == [1,1,1,1,1]
-- > hold [1,undefined] == [1,1]
hold :: [a] -> [a]
hold l =
    case l of
      [] -> []
      e:_ -> map (const e) l

-- | Interleave elements from two lists.  If one list ends the other
-- continues until it also ends.
--
-- > interleave2 [1,2,3,1,2,3] [4,5,6,7] == [1,4,2,5,3,6,1,7,2,3]
-- > [1..9] `isPrefixOf` interleave2 [1,3..] [2,4..]
interleave2 :: [a] -> [a] -> [a]
interleave2 p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (x:xs,y:ys) -> x : y : interleave2 xs ys

-- | N-ary variant of 'interleave2', ie. 'concat' of 'transpose'.
--
-- > interleave [whitei 'α' 0 4 3,whitei 'β' 5 9 3] == [2,7,2,6,0,8]
-- > [1..9] `isPrefixOf` interleave [[1,4..],[2,5..],[3,6..]]
interleave :: [[a]] -> [a]
interleave = concat . transpose

-- | Pattern where the 'tr' pattern determines the rate at which
-- values are read from the `x` pattern.  For each sucessive true
-- value at 'tr' the output is a (`Just` e) of each succesive element at
-- x.  False values at 'tr' generate `Nothing` values.
--
-- > let l = trigger (map toEnum [0,1,0,0,1,1]) [1,2,3]
-- > l == [Nothing,Just 1,Nothing,Nothing,Just 2,Just 3]
trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in concat (Collection.zipWith_c f r q)

-- * SC3 Patterns

-- | Pbrown.  SC3 pattern to generate psuedo-brownian motion.
--
-- > [7,2,8,6,2] `isPrefixOf` brown 'α' 0 9 15
brown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> [n]
brown e l r s = Stream.brown e (repeat l) (repeat r) (repeat s)

-- | PdurStutter.  SC3 pattern to partition a value into /n/ equal
-- subdivisions.  Subdivides each duration by each stutter and yields
-- that value stutter times.  A stutter of @0@ will skip the duration
-- value, a stutter of @1@ yields the duration value unaffected.
--
-- > let s = [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0]
-- > let d = [0.5,1,2,0.25,0.25]
-- > durStutter s d == [0.5,1.0,2.0,0.25,0.25]
durStutter :: Fractional a => [Int] -> [a] -> [a]
durStutter l =
    let f s d = case s of
                0 -> []
                1 -> [d]
                _ -> replicate s (d / fromIntegral s)
    in concat . zipWith f l

-- | Pexprand.  SC3 pattern of random values that follow a exponential
-- distribution.
--
-- > exprand 'α' 0.0001 1 10
exprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> [a]
exprand e l r n = Core.take_inf n (Stream.exprand e l r)

-- | Pfuncn.  Variant of the SC3 pattern that evaluates a closure at
-- each step that has a 'StdGen' form.
funcn :: Enum e => e -> (StdGen -> (n,StdGen)) -> Int -> [n]
funcn e = funcn' (mkStdGen (fromEnum e))

-- | Pgeom.  'Collection.geom' with arguments re-ordered.
--
-- > geom 3 6 5 == [3,18,108,648,3888]
geom :: Num a => a -> a -> Int -> [a]
geom i s n = Collection.geom n i s

-- | Pif.  Consume values from /q/ or /r/ according to /p/.
--
-- > if_demand [True,False,True] [1,3] [2] == [1,2,3]
if_demand :: [Bool] -> [a] -> [a] -> [a]
if_demand p q r =
    case if_rec (p,q,r) of
      Just (e,(p',q',r')) -> e : if_demand p' q' r'
      Nothing -> []

-- | Prand.  Random elements of /p/.
--
-- > rand' 'α' [1..9] 9 == [6,3,5,3,7,1,7,6,2]
rand' :: Enum e => e -> [a] -> Int -> [a]
rand' e a n = Core.take_inf n (Stream.rand e a)

-- | Pseq.  'concat' of 'replicate' of 'concat'.
--
-- > seq' [return 1,[2,3],return 4] 2 == [1,2,3,4,1,2,3,4]
seq' :: [[a]] -> Int -> [a]
seq' l n = concat (replicate n (concat l))

-- | Pslide.  SC3 pattern to slide over a list of values.
--
-- > slide [1,2,3,4] 4 3 1 0 True == [1,2,3,2,3,4,3,4,1,4,1,2]
-- > slide [1,2,3,4,5] 3 3 (-1) 0 True == [1,2,3,5,1,2,4,5,1]
slide :: [a] -> Int -> Int -> Int -> Int -> Bool -> [a]
slide a n j s i wr = concat (take n (Stream.slide a j s i wr))

-- | Pstutter.  Repeat each element of a pattern /n/ times.
--
-- > stutter [1,2,3] [4,5,6] == [4,5,5,6,6,6]
-- > stutter (repeat 2) [4,5,6] == [4,4,5,5,6,6]
stutter :: [Int] -> [a] -> [a]
stutter l = concat . zipWith replicate l

-- | Pswitch.  SC3 pattern to select elements from a list of patterns
-- by a pattern of indices.
--
-- > let r = switch [[1,2,3,1,2,3],[65,76],[800]] [2,2,0,1]
-- > r == [800,800,1,2,3,1,2,3,65,76]
switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

-- | Pswitch1.  SC3 pattern that uses a pattern of indices to select
-- which pattern to retrieve the next value from.  Only one value is
-- selected from each pattern.  This is in comparison to 'switch',
-- which embeds the pattern in its entirety.
--
-- > let p = switch1 [(cycle [1,2,3])
-- >                 ,(cycle [65,76])
-- >                 ,repeat 8] (concat (replicate 6 [2,2,0,1]))
-- > in p == [8,8,1,65,8,8,2,76,8,8,3,65,8,8,1,76,8,8,2,65,8,8,3,76]
switch1 :: [[a]] -> [Int] -> [a]
switch1 ps =
    let rec m l =
            case l of
              [] -> []
              i:l' -> case M.lookup i m of
                        Nothing -> []
                        Just [] -> []
                        Just (x:xs) -> x : rec (M.insert i xs m) l'
    in rec (M.fromList (zip [0..] ps))

-- | Pwhite.  SC3 pattern to generate a uniform linear distribution in
-- given range.
--
-- > white 'α' 0 9 5 == [1,8,9,4,4]
--
-- It is important to note that this structure is not actually
-- indeterminate, so that the below is zero.
--
-- > white 'α' 1 9 5  == [3,9,2,9,4]
-- > let p = white 'α' 0.0 1.0 3 in zipWith (-) p p == [0,0,0]
white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = Core.take_inf n (Stream.white e l r)

-- | Pwrand.  SC3 pattern to embed values randomly chosen from a list.
-- Returns one item from the list at random for each repeat, the
-- probability for each item is determined by a list of weights which
-- should sum to 1.0 and must be equal in length to the selection list.
--
-- > let w = SC3.normalizeSum [1,3,5]
-- > wrand 'ζ' [[1],[2],[3,4]] w 6 == [1,3,4,1,3,4,3,4,2]
wrand :: (Enum e,Fractional n,Ord n,Random n) =>
         e -> [[a]] -> [n] -> Int -> [a]
wrand e a w n = concat (Core.take_inf n (Stream.wrand_generic e a w))

-- | Pxrand.  SC3 pattern that is like 'rand' but filters successive
-- duplicates.
--
-- > xrand 'α' [return 1,[2,3],[4,5,6]] 9 == [4,5,6,2,3,4,5,6,1]
xrand :: Enum e => e -> [[a]] -> Int -> [a]
xrand e a n = Core.take_inf n (xrand' e a)

-- * SC3 Variant Patterns

-- | Underlying 'if_demand'.
if_rec :: ([Bool],[a],[a]) -> Maybe (a,([Bool],[a],[a]))
if_rec i =
    case i of
      (True:p,q:q',r) -> Just (q,(p,q',r))
      (False:p,q,r:r') -> Just (r,(p,q,r'))
      _ -> Nothing

-- | 'zip3' variant.
--
-- > if_zip [True,False,True] [1,3] [2] == [1]
if_zip :: [Bool] -> [a] -> [a] -> [a]
if_zip a b c =
    let f (x,y,z) = if x then y else z
    in map f (zip3 a b c)

-- | Underlying 'funcn'.
funcn' :: g -> (g -> (n,g)) -> Int -> [n]
funcn' g_ f n =
  let rec [] _ = []
      rec h g =
          case h of
            [] -> []
            e:h' -> let (r,g') = e g in r : rec h' g'
  in rec (replicate n f) g_

rorate_n' :: Num a => a -> a -> [a]
rorate_n' p i = [i * p,i * (1 - p)]

rorate_n :: Num a => [a] -> [a] -> [a]
rorate_n l = concat . zipWith rorate_n' l

rorate_l' :: Num a => [a] -> a -> [a]
rorate_l' p i = map (* i) p

rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate_l l = concat . zipWith rorate_l' l

-- | 'white' with pattern inputs.
--
-- > white' 'α' (repeat 0) [9,19,9,19,9,19] == [1,18,9,4,4,13]
white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (mapAccumL f g n)

-- | Type-specialised ('Integral') 'white'.
--
-- > whitei' 'α' 1 9 5 == [6,3,5,3,7]
whitei' :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
whitei' = white

-- | A variant of 'pwhite' that generates integral (rounded) values.
--
-- > whitei 'α' 1 9 5 == [6,5,2,7,8]
whitei :: (Random n,SC3.RealFracE n,Enum e) => e -> n -> n -> Int -> [n]
whitei e l r = fmap SC3.floorE . white e l r

-- | Underlying 'xrand'.
xrand' :: Enum e => e -> [[a]] -> [a]
xrand' e = concat . Stream.xrand e
