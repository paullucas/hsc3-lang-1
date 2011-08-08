{-# Language GeneralizedNewtypeDeriving #-}
module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Data.Foldable as F
import Data.List as L
import qualified Data.Map as M
import Data.Monoid
import Data.Traversable
import Sound.SC3.Lang.Collection.Numerical.Extending ()
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import System.Random

-- * P type and instances

newtype P a = P {unP :: [a]}
    deriving (Eq
             ,Functor
             ,Monoid
             ,Foldable
             ,Traversable
             ,Monad
             ,Num
             ,Fractional
             ,Floating
             ,Show)

instance Applicative P where
    pure = P . return
    P f <*> P e = P (map (\(f',e') -> f' e') (C.zip_c f e))

-- * P lifting

liftP :: ([a] -> [b]) -> P a -> P b
liftP f = P . f . toList

liftP2 :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2 f a b = P (f (toList a) (toList b))

liftP3 :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3 f a b c = P (f (toList a) (toList b) (toList c))

-- * P functions

pnull :: P a -> Bool
pnull = null . toList

psep :: P a -> (P a,P a)
psep (P p) =
    case p of
      [] -> (P [],P [])
      [e] -> (P [e],P [])
      (e:p') -> (P [e],P p')

fromList :: [a] -> P a
fromList = P

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = liftP (filter f)

reject :: (a -> Bool) -> [a] -> [a]
reject f = filter (not . f)

preject :: (a -> Bool) -> P a -> P a
preject f = liftP (reject f)

pconcat :: [P a] -> P a
pconcat = P . L.concat . map unP

seq :: [[a]] -> Int -> [a]
seq a i = L.concat (L.concat (replicate i a))

pseq :: [P a] -> Int -> P a
pseq a i = pconcat (L.concat (replicate i a))

pcycle :: P a -> P a
pcycle = liftP cycle

ser :: [[a]] -> Int -> [a]
ser a i = take i (cycle (L.concat a))

pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

concatReplicate :: Int -> [a] -> [a]
concatReplicate i = L.concat . replicate i

ln :: [a] -> Int -> [a]
ln = flip concatReplicate

pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = pconcat . replicate i

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

inf :: Int
inf = maxBound

ptake :: Int -> P a -> P a
ptake n = liftP (take n)

white :: (Random n, Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

pwhite :: (Random n, Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = P . white e l r

white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (L.mapAccumL f g n)

pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2 (white' e)

prepeat :: a -> P a
prepeat = P . repeat

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f = liftP2 (C.zipWith_c f)

pzip :: P a -> P b -> P (a,b)
pzip = liftP2 C.zip_c

countpre :: [Bool] -> [Int]
countpre =
    let f i p = if null p
                then if i == 0 then [] else [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if x then r else f (i + 1) xs
    in f 0

pcountpre :: P Bool -> P Int
pcountpre = liftP countpre

trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in L.concat (C.zipWith_c f r q)

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger = liftP2 trigger

-- | Functor bool
bool :: (Functor f, Ord a, Num a) => f a -> f Bool
bool = fmap (> 0)

countpost :: [Bool] -> [Int]
countpost =
    let f i p = if null p
                then [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if not x then f (i + 1) xs else r
    in tail . f 0

pcountpost :: P Bool -> P Int
pcountpost = liftP countpost

stutter :: [Int] -> [a] -> [a]
stutter ns = L.concat . C.zipWith_c replicate ns

pstutter :: P Int -> P a -> P a
pstutter = liftP2 stutter

clutch :: [a] -> [Bool] -> [a]
clutch p q =
    let r = map (+ 1) (countpost q)
    in stutter r p

pclutch :: P a -> P Bool -> P a
pclutch = liftP2 clutch

rand' :: Enum e => e -> [[a]] -> [a]
rand' e a =
    let k = length a - 1
        f g = let (i,g') = randomR (0,k) g
              in (a !! i) ++ f g'
    in f (mkStdGen (fromEnum e))

rand :: Enum e => e -> [[a]] -> Int -> [a]
rand e a n = take n (rand' e a)

prand :: Enum e => e -> [P a] -> Int -> P a
prand e a n = P (rand e (map unP a) n)

shuf :: Enum e => e -> [a] -> Int -> [a]
shuf e a n =
    let (a',_) = C.scramble' a (mkStdGen (fromEnum e))
    in concatReplicate n a'

pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a n = P (shuf e a n)

wrand' :: (Enum e,Random n,Ord n,Fractional n) => e -> [[a]] -> [n] -> [a]
wrand' e a w =
    let f g = let (r,g') = C.wchoose' a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrand :: (Enum e,Random n,Ord n,Fractional n) => e->[[a]]->[n]->Int->[a]
wrand e a w n = take n (wrand' e a w)

pwrand :: (Enum e,Random n,Ord n,Fractional n) => e->[P a]->[n]->Int->P a
pwrand e a w n = P (wrand e (map unP a) w n)

geom :: (Num a) => a -> a -> Int -> [a]
geom i s n = C.geom n i s

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s = P . geom i s

series :: (Num a) => a -> a -> Int -> [a]
series i s n = C.series n i s

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s = P . series i s

-- l < i <= r
wrap :: (Num a,Ord a) => (a,a) -> a -> a
wrap (l,r) i =
    let d = r - l
        f = wrap (l,r)
    in if i < l then f (i + d) else if i >= r then f (i - d) else i

{-
map (wrap (0,4)) [0..12]
-}

-- k = length a
segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (wrap (0,k)) [l .. r]
    in map (a !!) i

slide :: [a] -> Int -> Int -> Int -> Int -> Bool -> [a]
slide a n j s i wr =
    let k = length a
        l = enumFromThen i (i + s)
        r = map (+ (j - 1)) l
    in if wr
       then L.concat (take n (map (segment a k) (zip l r)))
       else error "slide: non-wrap variant not implemented"

pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide a n j s i = P . slide a n j s i

-- flop is strict!
lace :: [[a]] -> Int -> [a]
lace a n =
    let i = length a
    in take (n * i) (cycle (L.concat (C.flop a)))

place :: [P a] -> Int -> P a
place a n = P (lace (map unP a) n)

switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

pswitch :: [P a] -> P Int -> P a
pswitch l = liftP (switch (map unP l))

switch1 :: [[a]] -> [Int] -> [a]
switch1 ps =
    let go _ [] = []
        go m (i:is) = case M.lookup i m of
                        Nothing -> []
                        Just [] -> []
                        Just (x:xs) -> x : go (M.insert i xs m) is
    in go (M.fromList (zip [0..] ps))

pswitch1 :: [P a] -> P Int -> P a
pswitch1 l = liftP (switch1 (map unP l))

-- * Type specific aliases

pappend :: P a -> P a -> P a
pappend = mappend

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = bool

pempty :: P a
pempty = mempty
