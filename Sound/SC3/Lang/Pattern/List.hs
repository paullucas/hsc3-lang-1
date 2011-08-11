{-# Language GeneralizedNewtypeDeriving,FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.Plain where

import Control.Applicative
import Control.Monad
import Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import Data.Monoid
import Data.Traversable
import Sound.OpenSoundControl
import Sound.SC3.Server
import Sound.SC3.Lang.Collection.Event
import Sound.SC3.Lang.Collection.Numerical.Extending ()
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import Sound.SC3.Lang.Math.Datum ()
import qualified Sound.SC3.Lang.Math.Pitch as P
import qualified Sound.SC3.Lang.Math.SimpleNumber as N
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

inf :: Int
inf = maxBound

-- * P lifting

liftP :: ([a] -> [b]) -> P a -> P b
liftP f = fromList . f . toList

liftP2 :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2 f a b = P (f (toList a) (toList b))

liftP3 :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3 f a b c = P (f (toList a) (toList b) (toList c))

-- * P functions

pnull :: P a -> Bool
pnull = null . toList

fromList :: [a] -> P a
fromList = P

prepeat :: a -> P a
prepeat = fromList . repeat

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f = liftP2 (C.zipWith_c f)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f = liftP3 (C.zipWith3_c f)

pzip :: P a -> P b -> P (a,b)
pzip = liftP2 C.zip_c

-- * SC3 patterns

padd :: Num a => Key -> P a -> P (Event a) -> P (Event a)
padd k p = pzipWith (\i j -> e_edit k (+ i) j) p

bindTruncating :: [(Key,[Datum])] -> [Event Datum]
bindTruncating xs =
    let xs' = map (\(k,v) -> zip (repeat k) v) xs
    in map e_from_list (L.transpose xs')

bindExtending :: [(Key,[Datum])] -> [Event Datum]
bindExtending xs =
    let xs' = map (\(k,v) -> zip (repeat k) v) xs
    in map e_from_list (C.flop xs')

pbind :: [(String,P Datum)] -> P (Event Datum)
pbind xs = P (bindExtending (map (\(k,v) -> (k,unP v)) xs))

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pconst :: (Ord a,Num a) => a -> P a -> a -> P a
pconst n p t =
    let f _ [] = []
        f j (i:is) = if i + j < n - t
                     then i : f (j + i) is
                     else [n - j]
    in fromList (f 0 (unP p))

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 P.degree_to_key

durStutter :: Fractional a => [Int] -> [a] -> [a]
durStutter p =
    let f s d = case s of
                0 -> []
                1 -> [d]
                _ -> replicate s (d / fromIntegral s)
    in L.concat . C.zipWith_c f p

pdurStutter :: Fractional a => P Int -> P a -> P a
pdurStutter = liftP2 durStutter

pedit :: Key -> (a -> a) -> P (Event a) -> P (Event a)
pedit k f = fmap (e_edit k f)

pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (N.exprand l r) (pwhite e l r n)

pfinval :: Int -> P a -> P a
pfinval = ptake

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = P (C.geom n i s)

ifF :: Bool -> a -> a -> a
ifF x y z = if x then y else z

ifF' :: (Bool,a,a) -> a
ifF' (x,y,z) = if x then y else z

ifTruncating :: [Bool] -> [a] -> [a] -> [a]
ifTruncating  a b c = map ifF' (zip3 a b c)

ifExtending :: [Bool] -> [a] -> [a] -> [a]
ifExtending a b c = map ifF' (C.zip3_c a b c)

pif :: P Bool -> P a -> P a -> P a
pif = liftP3 ifExtending

place :: [P a] -> Int -> P a
place a n =
    let i = length a
    in P (take (n * i) (cycle (L.concat (C.flop (map unP a)))))

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

prand' :: Enum e => e -> [P a] -> P a
prand' e a =
    let a' = map unP a
        k = length a - 1
        f g = let (i,g') = randomR (0,k) g
              in (a' !! i) ++ f g'
    in P (f (mkStdGen (fromEnum e)))

prand :: Enum e => e -> [P a] -> Int -> P a
prand e a n = ptake n (prand' e a)

preject :: (a -> Bool) -> P a -> P a
preject f = liftP (filter (not . f))

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
      Left p' -> fromList . rorate_n' p'
      Right p' -> fromList . rorate_l' p'

prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate p = join . pzipWith prorate' p

pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

pseq :: [P a] -> Int -> P a
pseq a i = pconcat (L.concat (replicate i a))

pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (C.series n i s)

pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a =
    let (a',_) = C.scramble' a (mkStdGen (fromEnum e))
    in pn (P a')

wrap' :: (Num a,Ord a) => (a,a) -> a -> a
wrap' (l,r) i =
    let d = r - l
        f = wrap' (l,r)
    in if i < l then f (i + d) else if i >= r then f (i - d) else i

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
pslide a n j s i = fromList . slide a n j s i

stutterTruncating :: [Int] -> [a] -> [a]
stutterTruncating ns = L.concat . zipWith replicate ns

stutterExtending :: [Int] -> [a] -> [a]
stutterExtending ns = L.concat . C.zipWith_c replicate ns

pstutter :: P Int -> P a -> P a
pstutter = liftP2 stutterExtending

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

white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (L.mapAccumL f g n)

pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2 (white' e)

white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = fromList . white e l r

wrand' :: (Enum e) =>e -> [[a]] -> [Double] -> [a]
wrand' e a w =
    let f g = let (r,g') = C.wchoose' a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrand :: (Enum e) => e -> [[a]] -> [Double] -> Int -> [a]
wrand e a w n = take n (wrand' e a w)

pwrand :: Enum e => e -> [P a] -> [Double] -> Int -> P a
pwrand e a w n = P (wrand e (map unP a) w n)

pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap xs l r = fmap (wrap' (l,r)) xs

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

-- * Monoid aliases

pappend :: P a -> P a -> P a
pappend = mappend

pconcat :: [P a] -> P a
pconcat = mconcat

pempty :: P a
pempty = mempty

-- * Monad aliases

pjoin :: P (P a) -> P a
pjoin = join

-- * Data.List functions

pcycle :: P a -> P a
pcycle = liftP cycle

pdrop :: Int -> P a -> P a
pdrop n = liftP (drop n)

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = liftP (filter f)

preplicate :: Int -> a -> P a
preplicate n = fromList . replicate n

-- | Data.List.tail is partial
ptail :: P a -> P a
ptail = pdrop 1

ptake :: Int -> P a -> P a
ptake n = liftP (take n)

-- * Non-SC3 patterns

pbool :: (Ord a,Num a) => P a -> P Bool
pbool = fmap (> 0)

pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = pconcat . replicate i

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

interleave :: [a] -> [a] -> [a]
interleave p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (x:xs,y:ys) -> x : y : interleave xs ys

pinterleave :: P a -> P a -> P a
pinterleave = liftP2 interleave

rsd :: (Eq a) => [a] -> [a]
rsd q =
    case q of
      (i:is) -> let f n p = case p of
                              [] -> []
                              x:xs -> if x == n then f x xs else x : f x xs
                in i : f i is
      _ -> q

prsd :: Eq a => P a -> P a
prsd = liftP rsd

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

instance Audible (P (Event Datum)) where
    play fd = e_play fd . unP
