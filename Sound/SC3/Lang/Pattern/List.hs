-- | List variants of @SC3@ pattern functions.
module Sound.SC3.Lang.Pattern.List where

import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Monoid {- base -}
import Data.List {- base -}
import qualified Sound.SC3 as S {- hsc3 -}
import System.Random {- random -}

import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Math as M
import qualified Sound.SC3.Lang.Random.Gen as R

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
-- The underlying primitive is the 'S.fold_' function.
--
-- > let f n = S.fold_ n (-7) 11
-- > in map f [10,11,12,-6,-7,-8] == [10,11,10,-6,-7,-6]
ffold :: (Functor f,Num a,Ord a) => f a -> a -> a -> f a
ffold p i j = fmap (\n -> S.fold_ n i j) p

-- | SC3 pattern to constrain the range of output values by wrapping,
-- the primitive is 'S.genericWrap'.
--
-- > let p = fmap round (fwrap (geom 200 1.2 10) 200 1000)
-- > in p == [200,240,288,346,415,498,597,717,860,231]
fwrap :: (Functor f,Ord a,Num a) => f a -> a -> a -> f a
fwrap xs l r = fmap (S.genericWrap l r) xs

-- * Data.List variants

-- | Inverse of 'Data.List.:'.
--
-- > map uncons [[],1:[]] == [(Nothing,[]),(Just 1,[])]
uncons :: [a] -> (Maybe a,[a])
uncons l =
    case l of
      [] -> (Nothing,[])
      x:l' -> (Just x,l')

-- | 'Maybe' variant of '!!'.
--
-- > map (lindex "str") [2,3] == [Just 'r',Nothing]
lindex :: [a] -> Int -> Maybe a
lindex l n =
    if n < 0
    then Nothing
    else case (l,n) of
           ([],_) -> Nothing
           (x:_,0) -> Just x
           (_:l',_) -> lindex l' (n - 1)

-- | If /n/ is 'maxBound' this is 'id', else it is 'take'.
take_inf :: Int -> [a] -> [a]
take_inf n = if n == maxBound then id else take n

-- | Variant of 'transpose' for /fixed width/ interior lists.  Holes
-- are represented by 'Nothing'.
--
-- > transpose_fw undefined [] == []
--
-- > transpose [[1,3],[2,4]] == [[1,2],[3,4]]
-- > transpose_fw 2 [[1,3],[2,4]] == [[Just 1,Just 2],[Just 3,Just 4]]
--
-- > transpose [[1,5],[2],[3,7]] == [[1,2,3],[5,7]]
--
-- > transpose_fw 2 [[1,4],[2],[3,6]] == [[Just 1,Just 2,Just 3]
-- >                                     ,[Just 4,Nothing,Just 6]]
--
-- This function is more productive than 'transpose' for the case of
-- an infinite list of finite lists.
--
-- > map head (transpose_fw 4 (repeat [1..4])) == map Just [1,2,3,4]
-- > map head (transpose (repeat [1..4])) == _|_
transpose_fw :: Int -> [[a]] -> [[Maybe a]]
transpose_fw w l =
    if null l
    then []
    else let f n = map (`lindex` n) l
         in map f [0 .. w - 1]

-- | Variant of 'transpose_fw' with default value for holes.
transpose_fw_def :: a -> Int -> [[a]] -> [[a]]
transpose_fw_def def w l =
    let f n = map (fromMaybe def . (`lindex` n)) l
    in map f [0 .. w - 1]

-- | Variant of 'transpose_fw_def' deriving /width/ from first element.
transpose_fw_def' :: a -> [[a]] -> [[a]]
transpose_fw_def' def l =
    case l of
      [] -> []
      h:_ -> transpose_fw_def def (length h) l

-- | A 'transpose' variant, halting when first hole appears.
--
-- > trs [[1,2,3],[4,5,6],[7,8]] == [[1,4,7],[2,5,8]]
transpose_st :: [[a]] -> [[a]]
transpose_st l =
    let (h,l') = unzip (map uncons l)
    in case all_just h of
         Just h' -> h' : transpose_st l'
         Nothing -> []

-- * Data.Maybe variants

-- | Variant of 'catMaybes' that returns 'Nothing' unless /all/
-- elements are 'Just'.
--
-- > map all_just [[Nothing,Just 1],[Just 0,Just 1]] == [Nothing,Just [0,1]]
all_just :: [Maybe a] -> Maybe [a]
all_just =
    let rec r l =
            case l of
              [] -> Just (reverse r)
              Nothing:_ -> Nothing
              Just e:l' -> rec (e:r) l'
    in rec []

-- * Data.Monoid variants

-- | 'mconcat' of 'repeat', for lists this is 'cycle'.
--
-- > [1,2,3,1,2] `isPrefixOf` take 5 (mcycle [1,2,3])
mcycle :: Monoid a => a -> a
mcycle = mconcat . repeat

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
-- > interleave2 (seq' [1,2,3] 2) [4,5,6,7] == [1,4,2,5,3,6,1,7,2,3]
-- > [1..9] `isPrefixOf` interleave2 [1,3..] [2,4..]
interleave2 :: [a] -> [a] -> [a]
interleave2 p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (x:xs,y:ys) -> x : y : interleave2 xs ys

-- | N-ary variant of 'interleave2', ie. 'concat' of 'transpose'.
--
-- > interleave [whitei 'α' 0 4 3,whitei 'β' 5 9 3] == [3,7,0,8,1,6]
-- > [1..9] `isPrefixOf` interleave [[1,4..],[2,5..],[3,6..]]
interleave :: [[a]] -> [a]
interleave = concat . transpose

-- | Remove successive duplicates.
--
-- > rsd (stutter (repeat 2) [1,2,3]) == [1,2,3]
-- > rsd (seq' [1,2,3] 2) == [1,2,3,1,2,3]
rsd :: (Eq a) => [a] -> [a]
rsd =
    let f (p,_) i = (Just i,if Just i == p then Nothing else Just i)
    in mapMaybe snd . scanl f (Nothing,Nothing)

-- | Pattern where the 'tr' pattern determines the rate at which
-- values are read from the `x` pattern.  For each sucessive true
-- value at 'tr' the output is a (`Just` e) of each succesive element at
-- x.  False values at 'tr' generate `Nothing` values.
--
-- > let l = trigger (map toEnum [0,1,0,0,1,1]) [1,2,3]
-- > in l == [Nothing,Just 1,Nothing,Nothing,Just 2,Just 3]
trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in concat (C.zipWith_c f r q)

-- * SC3 Patterns

-- | Underlying 'brown''.
brown_ :: (RandomGen g,Random n,Num n,Ord n) => (n,n,n) -> (n,g) -> (n,g)
brown_ (l,r,s) (n,g) =
    let (i,g') = randomR (-s,s) g
    in (S.foldToRange l r (n + i),g')

-- | Brown noise with list inputs.
--
-- > let l = brown' 'α' (repeat 1) (repeat 700) (cycle [1,20])
-- > in [415,419,420,428] `isPrefixOf` l
brown' :: (Enum e,Random n,Num n,Ord n) => e -> [n] -> [n] -> [n] -> [n]
brown' e l_ r_ s_ =
    let i = (randomR (head l_,head r_) (mkStdGen (fromEnum e)))
        rec (n,g) z =
            case z of
              [] -> []
              (l,r,s):z' -> let (n',g') = brown_ (l,r,s) (n,g)
                            in n' : rec (n',g') z'
    in rec i (zip3 l_ r_ s_)

-- | SC3 pattern to generate psuedo-brownian motion.
--
-- > [4,4,1,8,5] `isPrefixOf` brown 'α' 0 9 15
brown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> [n]
brown e l r s = brown' e (repeat l) (repeat r) (repeat s)

-- | SC3 pattern to partition a value into /n/ equal subdivisions.
-- Subdivides each duration by each stutter and yields that value
-- stutter times.  A stutter of @0@ will skip the duration value, a
-- stutter of @1@ yields the duration value unaffected.
--
-- > let {s = [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0]
-- >     ;d = [0.5,1,2,0.25,0.25]}
-- > in durStutter s d == [0.5,1.0,2.0,0.25,0.25]
durStutter :: Fractional a => [Int] -> [a] -> [a]
durStutter p =
    let f s d = case s of
                0 -> []
                1 -> [d]
                _ -> replicate s (d / fromIntegral s)
    in concat . zipWith f p

-- | An SC3 pattern of random values that follow a exponential
-- distribution.
--
-- > exprand 'α' 0.0001 1 10
exprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> [a]
exprand e l r n = fmap (M.exprange l r) (white e 0 1 n)

-- | Underlying 'funcn'.
funcn' :: (RandomGen g) => g -> (g -> (n,g)) -> Int -> [n]
funcn' g_ f n =
  let rec [] _ = []
      rec h g =
          case h of
            [] -> []
            e:h' -> let (r,g') = e g in r : rec h' g'
  in rec (replicate n f) g_

-- | Variant of the SC3 pattern that evaluates a closure at each step
-- that has a 'StdGen' form.
funcn :: Enum e => e -> (StdGen -> (n,StdGen)) -> Int -> [n]
funcn e = funcn' (mkStdGen (fromEnum e))

-- | 'C.geom' with arguments re-ordered.
--
-- > geom 3 6 5 == [3,18,108,648,3888]
geom :: Num a => a -> a -> Int -> [a]
geom i s n = C.geom n i s

-- | Underlying 'if_demand'.
if_rec :: ([Bool],[a],[a]) -> Maybe (a,([Bool],[a],[a]))
if_rec i =
    case i of
      (True:p,q:q',r) -> Just (q,(p,q',r))
      (False:p,q,r:r') -> Just (r,(p,q,r'))
      _ -> Nothing

-- | Consume values from /q/ or /r/ according to /p/.
--
-- > if_demand [True,False,True] [1,3] [2] == [1,2,3]
if_demand :: [Bool] -> [a] -> [a] -> [a]
if_demand p q r =
    case if_rec (p,q,r) of
      Just (e,(p',q',r')) -> e : if_demand p' q' r'
      Nothing -> []

-- | 'zip3' variant.
--
-- > if_zip [True,False,True] [1,3] [2] == [1]
if_zip :: [Bool] -> [a] -> [a] -> [a]
if_zip a b c =
    let f (x,y,z) = if x then y else z
    in map f (zip3 a b c)

-- | Random elements of /p/.
--
-- > rand' 'α' [1..9] 9 == [3,9,2,9,4,7,4,3,8]
rand' :: Enum e => e -> [a] -> Int -> [a]
rand' e a n =
    let k = length a - 1
        i = white e 0 k n
    in map (a !!) i

rorate_n' :: Num a => a -> a -> [a]
rorate_n' p i = [i * p,i * (1 - p)]

rorate_n :: Num a => [a] -> [a] -> [a]
rorate_n p = concat . zipWith rorate_n' p

rorate_l' :: Num a => [a] -> a -> [a]
rorate_l' p i = map (* i) p

rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate_l p = concat . zipWith rorate_l' p

-- | List section with /wrapped/ indices.
--
-- > segment [0..4] 5 (3,5) == [3,4,0]
segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (S.genericWrap 0 (k - 1)) [l .. r]
    in map (a !!) i

-- | 'concat' of 'flip' 'replicate'.
seq' :: [a] -> Int -> [a]
seq' l = concat . flip replicate l

-- | SC3 pattern to slide over a list of values.
--
-- > slide [1,2,3,4] 4 3 1 0 True == [1,2,3,2,3,4,3,4,1,4,1,2]
-- > slide [1,2,3,4,5] 3 3 (-1) 0 True == [1,2,3,5,1,2,4,5,1]
slide :: [a] -> Int -> Int -> Int -> Int -> Bool -> [a]
slide a n j s i wr =
    let k = length a
        l = enumFromThen i (i + s)
        r = map (+ (j - 1)) l
    in if wr
       then concat (take n (map (segment a k) (zip l r)))
       else error "slide: non-wrap variant not implemented"

-- | Repeat each element of a pattern /n/ times.
--
-- > stutter [1,2,3] [4,5,6] == [4,5,5,6,6,6]
-- > stutter (repeat 2) [4,5,6] == [4,4,5,5,6,6]
stutter :: [Int] -> [a] -> [a]
stutter ns = concat . zipWith replicate ns

-- | Pattern to select elements from a list of patterns by a pattern
-- of indices.
--
-- > let r = switch [[1,2,3,1,2,3],[65,76],[800]] [2,2,0,1]
-- > in r == [800,800,1,2,3,1,2,3,65,76]
switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

-- | Pattern that uses a pattern of indices to select which pattern to
-- retrieve the next value from.  Only one value is selected from each
-- pattern.  This is in comparison to 'switch', which embeds the
-- pattern in its entirety.
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

-- | 'white' with pattern inputs.
--
-- > white' 'α' (repeat 0) (seq' [9,19] 3) == [3,0,1,6,6,15]
white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (mapAccumL f g n)

-- | SC3 pattern to generate a uniform linear distribution in given range.
--
-- > white 'α' 0 9 5 == [3,0,1,6,6]
--
-- It is important to note that this structure is not actually
-- indeterminate, so that the below is zero.
--
-- > white 'α' 1 9 5  == [3,9,2,9,4]
-- > let p = white 'α' 0.0 1.0 3 in zipWith (-) p p == [0,0,0]
white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take_inf n (randomRs (l,r) (mkStdGen (fromEnum e)))

-- | Type-specialised ('Integral') 'white'.
--
-- > whitei' 'α' 1 9 5 == [3,9,2,9,4]
whitei' :: (Random n,Integral n,Enum e) => e -> n -> n -> Int -> [n]
whitei' = white

-- | A variant of 'pwhite' that generates integral (rounded) values.
--
-- > whitei 'α' 1 9 5 == [5,1,7,7,8]
whitei :: (Random n,S.RealFracE n,Enum e) => e -> n -> n -> Int -> [n]
whitei e l r = fmap S.floorE . white e l r

-- | Underlying 'wrand'.
wrand' :: (Enum e,Fractional n,Ord n,Random n) => e -> [[a]] -> [n] -> [[a]]
wrand' e a w =
    let f g = let (r,g') = R.wchoose a w g
              in r : f g'
    in f (mkStdGen (fromEnum e))

-- | SC3 pattern to embed values randomly chosen from a list.  Returns
-- one item from the list at random for each repeat, the probability
-- for each item is determined by a list of weights which should sum
-- to 1.0.
--
-- > let w = C.normalizeSum [1,3,5]
-- > in wrand 'ζ' [[1],[2],[3,4]] w 6 == [3,4,2,2,3,4,1,3,4]
wrand :: (Enum e,Fractional n,Ord n,Random n) =>
         e -> [[a]] -> [n] -> Int -> [a]
wrand e a w n = concat (take_inf n (wrand' e a w))

-- | Underlying 'xrand'.
xrand' :: Enum e => e -> [[a]] -> [a]
xrand' e a =
    let k = length a - 1
        f j g = let (i,g') = randomR (0,k) g
                in if i == j then f j g' else (a !! i) ++ f i g'
    in f (-1) (mkStdGen (fromEnum e))

-- | SC3 pattern that is like 'rand' but filters successive duplicates.
--
-- > xrand 'α' [return 1,[2,3],[4,5,6]] 9 == [4,5,6,2,3,4,5,6,1]
xrand :: Enum e => e -> [[a]] -> Int -> [a]
xrand e a n = take_inf n (xrand' e a)
