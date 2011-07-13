module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import Data.Bool
import Data.Foldable
import Data.List hiding (concat,foldr)
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Prelude (Int,Eq(..),Ord(..),RealFrac(..),Num(..),Fractional(..),(.),error,Show(..))
import Sound.SC3.Identifier
import qualified Sound.SC3.Lang.Collection.Collection as S
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as S
import qualified Sound.SC3.Lang.Math.Pitch as S
import System.Random

data P a = P { unP :: [a] }

-- * Instances

instance Alternative P where
    empty = pempty
    (<|>) = pappend

instance Applicative P where
    pure = return
    (<*>) = ap

instance Foldable P where
    foldr = pfoldr

instance (Fractional a) => Fractional [a] where
    (/) = zipWith (/)
    recip = fmap recip
    fromRational = return . fromRational

instance (Fractional a) => Fractional (P a) where
    (/) = pzipWith (/)
    recip = fmap recip
    fromRational = return . fromRational

plift :: ([a] -> [b]) -> (P a -> P b)
plift f = P . f . unP

plift2 :: ([a] -> [b] -> [c]) -> (P a -> P b -> P c)
plift2 f x = P . f (unP x) . unP

instance Functor P where
    fmap f = plift (fmap f)

instance (Eq a) => Eq (P a) where
    (P p) == (P q) = p == q

instance Monad P where
    m >>= f = pconcatMap f m
    return x = P [x]

instance MonadPlus P where
    mzero = pempty
    mplus = pappend

instance Monoid (P a) where
    mempty = pempty
    mappend = pappend

instance (Num a) => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Num a) => Num (P a) where
    (+) = plift2 (+)
    (-) = plift2 (-)
    (*) = plift2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Show a) => Show (P a) where
    show = show . unP

instance Traversable P where
    traverse f = let cons_f x ys = pcons <$> f x <*> ys
                 in pfoldr cons_f (pure pempty)

-- * Basic constructors

inf :: Monad m => m Int
inf = return 83886028 -- 2 ^^ 23

pinf :: P Int
pinf = inf

-- * List functions

headM :: [a] -> Maybe a
headM xs =
    case xs of
      [] -> Nothing
      x:_ -> Just x

bool :: (Functor f, Ord a, Num a) => f a -> f Bool
bool = fmap (> 0)

clutch :: [a] -> [Bool] -> [a]
clutch p q =
    let r = fmap (+ 1) (countpost q)
    in stutter r p

-- | Count false values following each true value.
countpost :: [Bool] -> [Int]
countpost =
    let f i [] = [i]
        f i (x:xs) = if not x
                     then f (i + 1) xs
                     else i : f 0 xs
    in tail . f 0

-- | Count false values preceding each true value.
countpre :: [Bool] -> [Int]
countpre =
    let f i [] = if i == 0 then [] else [i]
        f i (x:xs) = if x
                     then i : f 0 xs
                     else f (i + 1) xs
    in f 0

interleave :: [a] -> [a] -> [a]
interleave xs ys =
    case (xs,ys) of
      (_,[]) -> xs
      ([],_) -> ys
      (p:ps,q:qs) -> p : q : interleave ps qs

-- | Remove successive duplicates.
rsd :: (Eq a) => [a] -> [a]
rsd =
    let f _ [] = []
        f Nothing (x:xs) = x : f (Just x) xs
        f (Just y) (x:xs) = if x == y
                            then f (Just x) xs
                            else x : f (Just x) xs
    in f Nothing

stutter :: [Int] -> [a] -> [a]
stutter ns = concat . zipWith replicate ns

trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in concat (zipWith f r q)

-- * Pattern functions

pappend :: P a -> P a -> P a
pappend = plift2 (++)

papply :: P (a -> b) -> P a -> P b
papply (P f) (P x) = P (f <*> x)

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = bool

pclutch :: P a -> P Bool -> P a
pclutch (P x) (P c) = P (clutch x c)

collect :: (a -> b) -> [a] -> [b]
collect = fmap

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pcountpost :: P Bool -> P Int
pcountpost = plift countpost

pcountpre :: P Bool -> P Int
pcountpre = plift countpre

pconcat :: P (P a) -> P a
pconcat p =
    if pnull p
    then pempty
    else case pheadM p of
           Nothing -> pempty
           Just x -> x `pappend` (pconcat (ptail p))

pconcatMap :: (b -> P a) -> P b -> P a
pconcatMap f = pconcat . fmap f

pcons :: a -> P a -> P a
pcons x = plift (x:)

pcycle :: P a -> P a
pcycle = plift cycle

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 S.degree_to_key

pdrop :: P Int -> P a -> P a
pdrop n =
    case pheadM n of
      Nothing -> error "pdrop"
      Just n' -> plift (drop n')

pempty :: P a
pempty = P []

pfilter :: (a -> Bool) -> P a -> P a
pfilter = plift . filter

fin :: [Int] -> [a] -> [a]
fin ns = take (head ns)

pfin :: P Int -> P a -> P a
pfin = ptake

pfoldr :: (a -> b -> b) -> b -> P a -> b
pfoldr f x = foldr f x . unP

geom :: (Num a) => a -> a -> Int -> [a]
geom i s n = S.geom n i s

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s = P . geom i s

pheadM :: P a -> Maybe a
pheadM = headM . unP

pinterleave :: P a -> P a -> P a
pinterleave (P p) (P q) = P (interleave p q)

nof :: [a] -> [Int] -> [a]
nof xs is =
    case is of
      [] -> error "nof"
      i:_ -> concat (take i (repeat xs))

pn :: P a -> P Int -> P a
pn = plift2 nof

pnull :: P a -> Bool
pnull = null . unP

prepeat :: a -> P a
prepeat = P . repeat

reject :: (a -> Bool) -> [a] -> [a]
reject f =
    let g i _ = f i
    in S.reject g

preject :: (a -> Bool) -> P a -> P a
preject f = plift (reject f)

prsd :: (Eq a) => P a -> P a
prsd = plift rsd

seq :: [[a]] -> [Int] -> [a]
seq ps n =
    case headM n of
      Nothing -> error "seq: empty repeat pattern"
      Just n' -> let ps' = concat (replicate n' ps)
                 in foldr mappend mempty ps'

pseq :: [P a] -> P Int -> P a
pseq ps n =
    case pheadM n of
      Nothing -> error "pseq: empty repeat pattern"
      Just n' -> let ps' = concat (replicate n' ps)
                 in foldr mappend mempty ps'

ser :: [[a]] -> [Int] -> [a]
ser ps n = fin n (seq ps inf)

pser :: [P a] -> P Int -> P a
pser ps n = ptake n (pseq ps pinf)

series :: (Num a) => a -> a -> Int -> [a]
series i s n = S.series n i s

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (S.series n i s)

pstutter :: P Int -> P a -> P a
pstutter = plift2 stutter

switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

switch1 :: [[a]] -> [Int] -> [a]
switch1 ps is =
    case is of
      [] -> []
      i:_ -> let (l,r) = splitAt i ps
                 (p:_) = r
                 j = tail is
             in case p of
                  [] -> switch1 ps j
                  x:_ -> let ps' = l ++ [tail p] ++ tail r
                         in x : switch1 ps' j

pswitch1 :: [P a] -> P Int -> P a
pswitch1 ps i =
    case pheadM i of
      Nothing -> pempty
      Just i' -> let (l, r) = splitAt i' ps
                     (p:_) = r
                     x = pheadM p
                     j = ptail i
                 in case x of
                      Nothing -> pswitch1 ps j
                      Just x' -> let ps' = l ++ [ptail p] ++ tail r
                                 in x' `pcons` pswitch1 ps' j

tail' :: [a] -> [a]
tail' xs = if null xs then [] else tail xs

ptail :: P a -> P a
ptail = plift tail'

ptake :: P Int -> P a -> P a
ptake n =
    case pheadM n of
      Nothing -> error "ptake: empty length pattern"
      Just n' -> plift (take n')

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger (P p) (P q) = P (trigger p q)

pzip :: P a -> P b -> P (a, b)
pzip (P p) (P q) = P (zip p q)

pzip3 :: P a -> P b -> P c -> P (a, b, c)
pzip3 (P p) (P q) (P r) = P (zip3 p q r)

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f (P p) (P q) = P (zipWith f p q)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f (P p) (P q) (P r) = P (zipWith3 f p q r)

-- * Random patterns

choosea :: StdGen -> A.Array Int a -> [a]
choosea g r =
    let (i, g') = randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

choose :: ID n => n -> [a] -> [a]
choose n p =
    let g = mkStdGen (resolveID n)
    in choosea g (A.listArray (0, length p - 1) p)

pchoose :: ID n => n -> P a -> P a
pchoose n = plift (choose n)

pnoise :: ID n => (Random a) => n -> P a
pnoise n =
    let g = mkStdGen (resolveID n)
    in P (randoms g)

rand :: ID n => n -> [[a]] -> [Int] -> [a]
rand s ps ns =
    case ns of
      [] -> error "rand"
      n:_ -> let g = mkStdGen (resolveID s)
                 qs = choosea g (A.listArray (0, length ps - 1) ps)
             in foldr (++) [] (take n qs)

prand :: ID n => n -> [P a] -> P Int -> P a
prand s ps n =
    case pheadM n of
      Nothing -> error "prand"
      Just n' -> let g = mkStdGen (resolveID s)
                     qs = choosea g (A.listArray (0, length ps - 1) ps)
                 in foldr pappend pempty (take n' qs)

rand_b :: (Random a) => StdGen -> [(a,a)] -> [a]
rand_b g b =
    case headM b of
      Nothing -> mempty
      Just b' -> let (x, g') = randomR b' g
                 in x : (rand_b g' (tail b))

prand_b :: (Random a) => StdGen -> P (a,a) -> P a
prand_b g = plift (rand_b g)

white :: (ID n,Random a) => n -> [a] -> [a] -> [a]
white n l r =
    let b = zip (cycle l) (cycle r)
        g = mkStdGen (resolveID n)
    in rand_b g b

pwhite :: (ID n,Random a) => n -> P a -> P  a -> P a
pwhite n = plift2 (white n)

-- * Extension

class Extending f where
    zipWith_c :: (a -> b -> c) -> f a -> f b -> f c

instance Extending P where
    zipWith_c f p = pzipWith f p . pcycle

instance Extending [] where
    zipWith_c f p = zipWith f p . cycle

(+.) :: (Extending f,Num a) => f a -> f a -> f a
(+.) = zipWith_c (+)

(*.) :: (Extending f,Num a) => f a -> f a -> f a
(*.) = zipWith_c (*)

(/.) :: (Extending f,Fractional a) => f a -> f a -> f a
(/.) = zipWith_c (/)

(-.) :: (Extending f,Num a) => f a -> f a -> f a
(-.) = zipWith_c (-)
