{-# Language FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.List where

import Data.List as L
import qualified Data.Map as M
import Sound.OpenSoundControl
import Sound.SC3.Server
import Sound.SC3.Lang.Collection.Event
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import Sound.SC3.Lang.Math.Datum ()
import qualified Sound.SC3.Lang.Math.SimpleNumber as N
import System.Random

-- * Utilities

inf :: Int
inf = maxBound

ifF :: Bool -> a -> a -> a
ifF x y z = if x then y else z

ifF' :: (Bool,a,a) -> a
ifF' (x,y,z) = if x then y else z

-- * SC3 patterns

bindTruncating :: [(Key,[Datum])] -> [Event Datum]
bindTruncating xs =
    let xs' = map (\(k,v) -> zip (repeat k) v) xs
    in map e_from_list (transpose xs')

bindExtending :: [(Key,[Datum])] -> [Event Datum]
bindExtending xs =
    let xs' = map (\(k,v) -> zip (repeat k) v) xs
    in map e_from_list (C.flop xs')

bind :: [(Key,[Datum])] -> [Event Datum]
bind = bindExtending

clutch :: [a] -> [Bool] -> [a]
clutch p q =
    let r = map (+ 1) (countpost q)
    in stutter r p

exprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> [a]
exprand e l r n = fmap (N.exprand l r) (white e l r n)

finval :: Int -> [a] -> [a]
finval = take

geom :: (Num a) => a -> a -> Int -> [a]
geom i s n = C.geom n i s

ifTruncating :: [Bool] -> [a] -> [a] -> [a]
ifTruncating  a b c = map ifF' (zip3 a b c)

ifExtending :: [Bool] -> [a] -> [a] -> [a]
ifExtending a b c = map ifF' (C.zip3_c a b c)

lif :: [Bool] -> [a] -> [a] -> [a]
lif = ifExtending

lace :: [[a]] -> Int -> [a]
lace a n =
    let i = length a
    in take (n * i) (cycle (L.concat (C.flop a)))

ln :: [a] -> Int -> [a]
ln = flip concatReplicate

rand' :: Enum e => e -> [[a]] -> [a]
rand' e a =
    let k = length a - 1
        f g = let (i,g') = randomR (0,k) g
              in (a !! i) ++ f g'
    in f (mkStdGen (fromEnum e))

rand :: Enum e => e -> [[a]] -> Int -> [a]
rand e a n = take n (rand' e a)

reject :: (a -> Bool) -> [a] -> [a]
reject f = filter (not . f)

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

seq :: [[a]] -> Int -> [a]
seq a i = L.concat (L.concat (replicate i a))
ser :: [[a]] -> Int -> [a]
ser a i = take i (cycle (L.concat a))

series :: (Num a) => a -> a -> Int -> [a]
series i s n = C.series n i s

shuf :: Enum e => e -> [a] -> Int -> [a]
shuf e a n =
    let (a',_) = C.scramble' a (mkStdGen (fromEnum e))
    in concatReplicate n a'

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

stutterTruncating :: [Int] -> [a] -> [a]
stutterTruncating ns = L.concat . zipWith replicate ns

stutterExtending :: [Int] -> [a] -> [a]
stutterExtending ns = L.concat . C.zipWith_c replicate ns

stutter :: [Int] -> [a] -> [a]
stutter = stutterExtending

switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

switch1 :: [[a]] -> [Int] -> [a]
switch1 ps =
    let go _ [] = []
        go m (i:is) = case M.lookup i m of
                        Nothing -> []
                        Just [] -> []
                        Just (x:xs) -> x : go (M.insert i xs m) is
    in go (M.fromList (zip [0..] ps))

white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (L.mapAccumL f g n)

white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

wrand'Generic :: (Enum e,Random n,Ord n,Fractional n) =>
                 e -> [[a]] -> [n] -> [a]
wrand'Generic e a w =
    let f g = let (r,g') = C.wchoose' a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrandGeneric :: (Enum e,Random n,Ord n,Fractional n) =>
                e -> [[a]] -> [n] -> Int -> [a]
wrandGeneric e a w n = take n (wrand'Generic e a w)

wrand :: (Enum e) => e -> [[a]] -> [Double] -> Int -> [a]
wrand = wrandGeneric

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

xrand' :: Enum e => e -> [[a]] -> [a]
xrand' e a =
    let k = length a - 1
        f j g = let (i,g') = randomR (0,k) g
                in if i == j then f j g' else (a !! i) ++ f i g'
    in f (-1) (mkStdGen (fromEnum e))

xrand :: Enum e => e -> [[a]] -> Int -> [a]
xrand e a n = take n (xrand' e a)

-- * Non-SC3 patterns

bool :: (Functor f,Ord a,Num a) => f a -> f Bool
bool = fmap (> 0)

concatReplicate :: Int -> [a] -> [a]
concatReplicate i = L.concat . replicate i

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
rsd q =
    case q of
      (i:is) -> let f n p = case p of
                              [] -> []
                              x:xs -> if x == n then f x xs else x : f x xs
                in i : f i is
      _ -> q

trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in L.concat (C.zipWith_c f r q)

-- * Pattern audition

-- t = time, dt = delta-time, i = node id, s = instrument name
-- rt = release time, a = parameters
e_osc :: Double -> Double -> Int -> Event Datum -> (OSC,OSC)
e_osc t dt i e =
    let (String s) = e_instrument e
        rt = dt * e_sustain' e
        f = e_freq e
        a = ("freq",f) : e_arg e
    in (Bundle (UTCr t) [s_new s i AddToTail 1 a]
       ,Bundle (UTCr (t+rt)) [n_set i [("gate",0)]])

e_play :: Transport t => t -> [Event Datum] -> IO ()
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

instance Audible [Event Datum] where
    play = e_play
