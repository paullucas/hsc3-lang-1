{-# Language GeneralizedNewtypeDeriving,FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Data.Foldable as F
import Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Sound.OpenSoundControl
import Sound.SC3.Server
import Sound.SC3.Lang.Collection.Event
import Sound.SC3.Lang.Collection.Numerical.Extending ()
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import qualified Sound.SC3.Lang.Math.Pitch as P
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

fromList :: [a] -> P a
fromList = P

inf :: Int
inf = maxBound

prepeat :: a -> P a
prepeat = P . repeat

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f = liftP2 (C.zipWith_c f)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f = liftP3 (C.zipWith3_c f)

pzip :: P a -> P b -> P (a,b)
pzip = liftP2 C.zip_c

-- * SC3 patterns

bind :: [(String,[Datum])] -> [Event]
bind xs =
    let xs' = map (\(k,v) -> zip (repeat k) v) xs
    in C.flop xs'

pbind :: [(String,P Datum)] -> P Event
pbind xs = P (bind (map (\(k,v) -> (k,unP v)) xs))

clutch :: [a] -> [Bool] -> [a]
clutch p q =
    let r = map (+ 1) (countpost q)
    in stutter r p

pclutch :: P a -> P Bool -> P a
pclutch = liftP2 clutch

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = liftA3 P.degree_to_key

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

exprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> [a]
exprand e l r n = fmap (to_exprand l r) (white e l r n)

pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (to_exprand l r) (pwhite e l r n)

finval :: Int -> [a] -> [a]
finval = take

pfinval :: Int -> P a -> P a
pfinval = ptake

geom :: (Num a) => a -> a -> Int -> [a]
geom i s n = C.geom n i s

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s = P . geom i s

ifF :: Bool -> a -> a -> a
ifF x y z = if x then y else z

ifF' :: (Bool,a,a) -> a
ifF' (x,y,z) = if x then y else z

lif :: [Bool] -> [a] -> [a] -> [a]
lif a b c = map ifF' (C.zip3_c a b c)

pif :: P Bool -> P a -> P a -> P a
pif = liftP3 lif

lace :: [[a]] -> Int -> [a]
lace a n =
    let i = length a
    in take (n * i) (cycle (L.concat (C.flop a)))

place :: [P a] -> Int -> P a
place a n = P (lace (map unP a) n)

concatReplicate :: Int -> [a] -> [a]
concatReplicate i = L.concat . replicate i

ln :: [a] -> Int -> [a]
ln = flip concatReplicate

pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = pconcat . replicate i

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

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

reject :: (a -> Bool) -> [a] -> [a]
reject f = filter (not . f)

preject :: (a -> Bool) -> P a -> P a
preject f = liftP (reject f)

rorate_n' :: Num a => a -> a -> [a]
rorate_n' p i = [i * p,i * (1 - p)]

rorate_n :: Num a => [a] -> [a] -> [a]
rorate_n p = L.concat . C.zipWith_c rorate_n' p

rorate_l' :: Num a => [a] -> a -> [a]
rorate_l' p i = map (* i) p

rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate_l p = L.concat . C.zipWith_c rorate_l' p

rorate' :: Num a => Either a [a] -> a -> [a]
rorate' p =
    case p of
      Left p' -> rorate_n' p'
      Right p' -> rorate_l' p'

rorate :: Num a => [Either a [a]] -> [a] -> [a]
rorate p = L.concat . C.zipWith_c rorate' p

prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate = liftP2 rorate

pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

seq :: [[a]] -> Int -> [a]
seq a i = L.concat (L.concat (replicate i a))

pseq :: [P a] -> Int -> P a
pseq a i = pconcat (L.concat (replicate i a))

ser :: [[a]] -> Int -> [a]
ser a i = take i (cycle (L.concat a))

pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

series :: (Num a) => a -> a -> Int -> [a]
series i s n = C.series n i s

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s = P . series i s

shuf :: Enum e => e -> [a] -> Int -> [a]
shuf e a n =
    let (a',_) = C.scramble' a (mkStdGen (fromEnum e))
    in concatReplicate n a'

pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a n = P (shuf e a n)

-- k = length a
segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (wrap' (0,k)) [l .. r]
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

stutter :: [Int] -> [a] -> [a]
stutter ns = L.concat . C.zipWith_c replicate ns

pstutter :: P Int -> P a -> P a
pstutter = liftP2 stutter

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

white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = P . white e l r

white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (L.mapAccumL f g n)

pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2 (white' e)

wrand'Generic :: (Enum e,Random n,Ord n,Fractional n) => e -> [[a]] -> [n] -> [a]
wrand'Generic e a w =
    let f g = let (r,g') = C.wchoose' a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrandGeneric :: (Enum e,Random n,Ord n,Fractional n) => e->[[a]]->[n]->Int->[a]
wrandGeneric e a w n = take n (wrand'Generic e a w)

pwrandGeneric :: (Enum e,Random n,Ord n,Fractional n) => e->[P a]->[n]->Int->P a
pwrandGeneric e a w n = P (wrandGeneric e (map unP a) w n)

pwrand :: Enum e => e -> [P a] -> [Double] -> Int -> P a
pwrand = pwrandGeneric

-- l < i <= r
wrap' :: (Num a,Ord a) => (a,a) -> a -> a
wrap' (l,r) i =
    let d = r - l
        f = wrap' (l,r)
    in if i < l then f (i + d) else if i >= r then f (i - d) else i

{-
map (wrap' (0,4)) [0..12]
-}

wrap :: (Num a,Ord a) => [a] -> a -> a -> [a]
wrap xs l r = map (wrap' (l,r)) xs

pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap xs l r = P (wrap (unP xs) l r)

xrand' :: Enum e => e -> [[a]] -> [a]
xrand' e a =
    let k = length a - 1
        f j g = let (i,g') = randomR (0,k) g
                in if i == j then f j g' else (a !! i) ++ f i g'
    in f (-1) (mkStdGen (fromEnum e))

xrand :: Enum e => e -> [[a]] -> Int -> [a]
xrand e a n = take n (xrand' e a)

pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = P (xrand e (map unP a) n)

-- * Type specific aliases

pappend :: P a -> P a -> P a
pappend = mappend

pempty :: P a
pempty = mempty

-- * Haskell/SC3 aliases

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pfilter :: (a -> Bool) -> P a -> P a
pfilter = pselect

ptake :: Int -> P a -> P a
ptake n = liftP (take n)

-- * Non-SC3 patterns

bool :: (Functor f,Ord a,Num a) => f a -> f Bool
bool = fmap (> 0)

pbool :: (Ord a,Num a) => P a -> P Bool
pbool = bool

pconcat :: [P a] -> P a
pconcat = P . L.concat . map unP

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

pcycle :: P a -> P a
pcycle = liftP cycle

trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in L.concat (C.zipWith_c f r q)

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger = liftP2 trigger

-- * Pattern audition

-- t = time, dt = delta-time, i = node id, s = instrument name
-- rt = release time, a = parameters
e_osc :: Double -> Double -> Int -> Event -> (OSC,OSC)
e_osc t dt i e =
    let s = e_instrument' e
        rt = dt * e_sustain' e
        f = e_freq e
        a = ("freq",f) : mapMaybe e_arg e
    in (Bundle (UTCr t) [s_new s i AddToTail 1 a]
       ,Bundle (UTCr (t+rt)) [n_set i [("gate",0)]])

e_play :: Transport t => t -> [Event] -> IO ()
e_play fd xs = do
  let act _ _ [] = return ()
      act _ [] _ = error "e_play: no id?"
      act t (i:is) (e:es) = do let dt = e_dur' e
                                   (p,q) = e_osc t dt i e
                               send fd p
                               send fd q
                               pauseThreadUntil (t + dt)
                               act (t + dt) is es
  st <- utcr
  act st [100..] xs

instance Audible [Event] where
    play = e_play

instance Audible (P Event) where
    play fd = e_play fd . unP
