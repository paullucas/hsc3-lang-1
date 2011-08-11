{-# Language FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.List where

import Data.List as L
import qualified Data.Map as M
import Sound.OpenSoundControl
import Sound.SC3.Server
import Sound.SC3.Lang.Collection.Event
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import Sound.SC3.Lang.Math.Datum ()
import qualified Sound.SC3.Lang.Math.Pitch as N
import qualified Sound.SC3.Lang.Math.SimpleNumber as N
import System.Random

type P a = [a]

fromList :: [a] -> P a
fromList = id

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

pbind :: [(Key,[Datum])] -> P (Event Datum)
pbind = bindExtending

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = map (+ 1) (pcountpost q)
    in pstutter r p

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = C.zipWith3_c N.degree_to_key

pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (N.exprand l r) (pwhite e l r n)

pfinval :: Int -> P a -> P a
pfinval = take

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = C.geom n i s

ifTruncating :: [Bool] -> [a] -> [a] -> [a]
ifTruncating  a b c = map ifF' (zip3 a b c)

ifExtending :: [Bool] -> [a] -> [a] -> [a]
ifExtending a b c = map ifF' (C.zip3_c a b c)

pif :: P Bool -> P a -> P a -> P a
pif = ifExtending

place :: [P a] -> Int -> P a
place a n =
    let i = length a
    in take (n * i) (cycle (L.concat (C.flop a)))

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

prand' :: Enum e => e -> [[a]] -> [a]
prand' e a =
    let k = length a - 1
        f g = let (i,g') = randomR (0,k) g
              in (a !! i) ++ f g'
    in f (mkStdGen (fromEnum e))

prand :: Enum e => e -> [P a] -> Int -> P a
prand e a n = take n (prand' e a)

preject :: (a -> Bool) -> P a -> P a
preject f = filter (not . f)

rorate_n' :: Num a => a -> a -> [a]
rorate_n' p i = [i * p,i * (1 - p)]

rorate_n :: Num a => [a] -> [a] -> [a]
rorate_n p = L.concat . C.zipWith_c rorate_n' p

rorate_l' :: Num a => [a] -> a -> [a]
rorate_l' p i = map (* i) p

rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate_l p = L.concat . C.zipWith_c rorate_l' p

prorate' :: Num a => Either a [a] -> a -> P a
prorate' p =
    case p of
      Left p' -> rorate_n' p'
      Right p' -> rorate_l' p'

prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate p = L.concat . C.zipWith_c prorate' p

pseq :: [P a] -> Int -> P a
pseq a i = L.concat (L.concat (replicate i a))

pser :: [P a] -> Int -> P a
pser a i = take i (cycle (L.concat a))

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = C.series n i s

pselect :: (a -> Bool) -> P a -> P a
pselect = filter

pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a n =
    let (a',_) = C.scramble' a (mkStdGen (fromEnum e))
    in pconcatReplicate n a'

-- k = length a
segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (wrap' (0,k)) [l .. r]
    in map (a !!) i

pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide a n j s i wr =
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

pstutter :: P Int -> P a -> P a
pstutter = stutterExtending

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pswitch1 :: [P a] -> P Int -> P a
pswitch1 ps =
    let go _ [] = []
        go m (i:is) = case M.lookup i m of
                        Nothing -> []
                        Just [] -> []
                        Just (x:xs) -> x : go (M.insert i xs m) is
    in go (M.fromList (zip [0..] ps))

pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (L.mapAccumL f g n)

pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

wrand'Generic :: (Enum e,Random n,Ord n,Fractional n) =>
                 e -> [[a]] -> [n] -> [a]
wrand'Generic e a w =
    let f g = let (r,g') = C.wchoose' a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrandGeneric :: (Enum e,Random n,Ord n,Fractional n) =>
                e -> [P a] -> [n] -> Int -> P a
wrandGeneric e a w n = take n (wrand'Generic e a w)

pwrand :: Enum e => e -> [P a] -> [Double] -> Int -> P a
pwrand = wrandGeneric

-- l < i <= r
wrap' :: (Num a,Ord a) => (a,a) -> a -> a
wrap' (l,r) i =
    let d = r - l
        f = wrap' (l,r)
    in if i < l then f (i + d) else if i >= r then f (i - d) else i

{-
map (wrap' (0,4)) [0..12]
-}

pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap xs l r = map (wrap' (l,r)) xs

pxrand' :: Enum e => e -> [P a] -> P a
pxrand' e a =
    let k = length a - 1
        f j g = let (i,g') = randomR (0,k) g
                in if i == j then f j g' else (a !! i) ++ f i g'
    in f (-1) (mkStdGen (fromEnum e))

pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = take n (pxrand' e a)

-- * Haskell/SC3 aliases

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pcycle :: P a -> P a
pcycle = cycle

pdrop :: Int -> P a -> P a
pdrop = drop

pfilter :: (a -> Bool) -> P a -> P a
pfilter = filter

ptake :: Int -> P a -> P a
ptake = take

-- * Non-SC3 patterns

pappend :: P a -> P a -> P a
pappend = (++)

pbool :: (Functor f,Ord a,Num a) => f a -> f Bool
pbool = fmap (> 0)

pconcatReplicate :: Int -> [a] -> [a]
pconcatReplicate i = L.concat . replicate i

pcountpost :: [Bool] -> [Int]
pcountpost =
    let f i p = if null p
                then [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if not x then f (i + 1) xs else r
    in tail . f 0

pcountpre :: [Bool] -> [Int]
pcountpre =
    let f i p = if null p
                then if i == 0 then [] else [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if x then r else f (i + 1) xs
    in f 0

pempty :: P a
pempty = []

pinterleave :: [a] -> [a] -> [a]
pinterleave p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (x:xs,y:ys) -> x : y : pinterleave xs ys

prepeat :: a -> [a]
prepeat = repeat

preplicate :: Int -> a -> P a
preplicate = replicate

prsd :: (Eq a) => [a] -> [a]
prsd q =
    case q of
      (i:is) -> let f n p = case p of
                              [] -> []
                              x:xs -> if x == n then f x xs else x : f x xs
                in i : f i is
      _ -> q

-- Data.List.tail is partial
ptail :: P a -> P a
ptail = drop 1

ptrigger :: [Bool] -> [a] -> [Maybe a]
ptrigger p q =
    let r = pcountpre p
        f i x = replicate i Nothing ++ [Just x]
    in L.concat (C.zipWith_c f r q)

pzip :: P a -> P b -> P (a,b)
pzip = C.zip_c

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith = C.zipWith_c

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
