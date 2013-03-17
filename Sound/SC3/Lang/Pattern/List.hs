-- | List variants of @SC3@ pattern functions.
module Sound.SC3.Lang.Pattern.List where

import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.List {- base -}
import qualified Sound.SC3 as S {- hsc3 -}
import System.Random {- random -}

import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Random.Gen as R

brown_ :: (RandomGen g,Random n,Num n,Ord n) => (n,n,n) -> (n,g) -> (n,g)
brown_ (l,r,s) (n,g) =
    let (i,g') = randomR (-s,s) g
    in (S.foldToRange l r (n + i),g')

brown' :: (Enum e,Random n,Num n,Ord n) => e -> [n] -> [n] -> [n] -> [n]
brown' e l_ r_ s_ =
    let go _ [] = []
        go (n,g) ((l,r,s):z) = let (n',g') = brown_ (l,r,s) (n,g)
                               in n' : go (n',g') z
    in go (randomR (head l_,head r_) (mkStdGen (fromEnum e))) (zip3 l_ r_ s_)

brown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> [n]
brown e l r s = brown' e (repeat l) (repeat r) (repeat s)

durStutter :: Fractional a => [Int] -> [a] -> [a]
durStutter p =
    let f s d = case s of
                0 -> []
                1 -> [d]
                _ -> replicate s (d / fromIntegral s)
    in concat . C.zipWith_c f p

ifF :: Bool -> a -> a -> a
ifF x y z = if x then y else z

ifF' :: (Bool,a,a) -> a
ifF' (x,y,z) = if x then y else z

ifTruncating :: [Bool] -> [a] -> [a] -> [a]
ifTruncating  a b c = map ifF' (zip3 a b c)

ifExtending :: [Bool] -> [a] -> [a] -> [a]
ifExtending a b c = map ifF' (C.zip3_c a b c)

rand' :: Enum e => e -> [a] -> Int -> [a]
rand' e a n =
    let k = length a - 1
        f m g = if m == 0
                then []
                else let (i,g') = randomR (0,k) g
                     in (a !! i) : f (m - 1) g'
    in f n (mkStdGen (fromEnum e))

rorate_n' :: Num a => a -> a -> [a]
rorate_n' p i = [i * p,i * (1 - p)]

rorate_n :: Num a => [a] -> [a] -> [a]
rorate_n p = concat . C.zipWith_c rorate_n' p

rorate_l' :: Num a => [a] -> a -> [a]
rorate_l' p i = map (* i) p

rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate_l p = concat . C.zipWith_c rorate_l' p

segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (S.genericWrap 0 k) [l .. r]
    in map (a !!) i

slide :: [a] -> Int -> Int -> Int -> Int -> Bool -> [a]
slide a n j s i wr =
    let k = length a
        l = enumFromThen i (i + s)
        r = map (+ (j - 1)) l
    in if wr
       then concat (take n (map (segment a k) (zip l r)))
       else error "slide: non-wrap variant not implemented"

stutterTruncating :: [Int] -> [a] -> [a]
stutterTruncating ns = concat . zipWith replicate ns

stutterExtending :: [Int] -> [a] -> [a]
stutterExtending ns = concat . C.zipWith_c replicate ns

switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

switch1 :: [[a]] -> [Int] -> [a]
switch1 ps =
    let go _ [] = []
        go m (i:is) = case M.lookup i m of
                        Nothing -> []
                        Just [] -> []
                        Just (x:xs) -> x : go (M.insert i xs m) is
    in go (M.fromList (zip [0..] (C.extendSequences ps)))

white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (mapAccumL f g n)

white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

wrand' :: (Enum e,Fractional n,Ord n,Random n) => e -> [[a]] -> [n] -> [a]
wrand' e a w =
    let f g = let (r,g') = R.wchoose a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrand :: (Enum e,Fractional n,Ord n,Random n) => e -> [[a]] -> [n] -> Int -> [a]
wrand e a w n = take n (wrand' e a w)

xrand' :: Enum e => e -> [[a]] -> [a]
xrand' e a =
    let k = length a - 1
        f j g = let (i,g') = randomR (0,k) g
                in if i == j then f j g' else (a !! i) ++ f i g'
    in f (-1) (mkStdGen (fromEnum e))

xrand :: Enum e => e -> [[a]] -> Int -> [a]
xrand e a n = take n (xrand' e a)

countpost :: [Bool] -> [Int]
countpost =
    let f i p = if null p
                then [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if not x then f (i + 1) xs else r
    in tail . f 0

countpre :: [Bool] -> [Int]
countpre =
    let f i p = if null p
                then if i == 0 then [] else [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if x then r else f (i + 1) xs
    in f 0

interleave :: [a] -> [a] -> [a]
interleave p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (x:xs,y:ys) -> x : y : interleave xs ys

rsd :: (Eq a) => [a] -> [a]
rsd =
    let f (p,_) i = (Just i,if Just i == p then Nothing else Just i)
    in mapMaybe snd . scanl f (Nothing,Nothing)

-- > let tr = map toEnum [0,0,1,0,0,0,1,1]
-- > in trigger tr [1,2,3]
trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in concat (C.zipWith_c f r q)

