{-# Language GeneralizedNewtypeDeriving #-}
module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import Data.Bool
import Data.Foldable hiding (toList)
import Data.List hiding (concat,foldr,find)
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Prelude hiding (concat,foldr)
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import qualified Sound.SC3.Lang.Math.Pitch as P
import System.Random

-- * List instances

newtype P a = P {toList :: [a]}
    deriving (Eq,Functor,Monoid,Foldable,Traversable,Monad,Show)

ppure :: a -> P a
ppure = P . repeat

prepeat :: a -> P a
prepeat = P . repeat

lapply :: [(a -> b)] -> [a] -> [b]
lapply p = map (\(f,e) -> f e) . zip p

papply :: P (a -> b) -> P a -> P b
papply (P f) (P e) = P (lapply f e)

instance Applicative P where
    pure = ppure
    (<*>) = papply

pmap :: (a -> b) -> P a -> P b
pmap = fmap

preturn :: a -> P a
preturn = return

pzip :: P a -> P b -> P (a, b)
pzip = pzipWith (,)

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f = liftA2 f

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f = liftA3 f

instance (Num a) => Num (P a) where
    (+) = pzipWith (+)
    (-) = pzipWith (-)
    (*) = pzipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional (P a) where
    (/) = pzipWith (/)
    recip = fmap recip
    fromRational = return . fromRational

-- * Non-pattern functions

-- | A very large positive integer
inf :: Int
inf = 83886028

-- | Choose an element of an array at random
choosea :: StdGen -> A.Array Int a -> [a]
choosea g r =
    let (i, g') = randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

headM :: [a] -> Maybe a
headM xs =
    case xs of
      [] -> Nothing
      x:_ -> Just x

rrand :: (Random a, Enum e) => e -> a -> a -> a
rrand e a b =
    let g = mkStdGen (fromEnum e)
    in fst (randomR (a,b) g)

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

fbool :: (Functor f, Ord a, Num a) => f a -> f Bool
fbool = fmap (> 0)

-- * Pattern functions

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fbool

psep :: P a -> (P a,P a)
psep (P p) =
    case p of
      [] -> (P [],P [])
      [e] -> (P [e],P [])
      (e:p') -> (P [e],P p')

phead :: P a -> P a
phead = fst . psep

ptail :: P a -> P a
ptail = snd . psep

pnull :: P a -> Bool
pnull = null . toList

ptake :: Int -> P a -> P a
ptake n = P . take n . toList

pdrop :: Int -> P a -> P a
pdrop n = P . drop n . toList

unp :: P a -> a
unp (P p) =
    case p of
      (e:_) -> e
      _ -> error "unp"

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost =
    let f i [] = [i]
        f i (x:xs) = if not x
                     then f (i + 1) xs
                     else i : f 0 xs
    in P . tail . f 0 . toList

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

interleave :: [a] -> [a] -> [a]
interleave xs ys =
    case (xs,ys) of
      (_,[]) -> xs
      ([],_) -> ys
      (p:ps,q:qs) -> p : q : interleave ps qs

pinterleave :: P a -> P a -> P a
pinterleave (P p) (P q) = P (interleave p q)

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 P.degree_to_key

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd =
    let f _ [] = []
        f Nothing (x:xs) = x : f (Just x) xs
        f (Just y) (x:xs) = if x == y
                            then f (Just x) xs
                            else x : f (Just x) xs
    in P . f Nothing . toList

preplicate :: Int -> a -> P a
preplicate n = P . replicate n

pcycle :: P a -> P a
pcycle = P . cycle . toList

pstutter :: P Int -> P a -> P a
pstutter ns = join . pzipWith preplicate (pcycle ns)

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre =
    let f i [] = if i == 0 then [] else [i]
        f i (x:xs) = if x
                     then i : f 0 xs
                     else f (i + 1) xs
    in P . f 0 . toList

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing `mappend` return (Just x)
    in join (pzipWith f r q)

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = P (C.geom n i s)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (C.series n i s)

pnil :: P a
pnil = P []

pn :: P a -> Int -> P a
pn p n = if n == 0 then mempty else p `mappend` (pn p (n - 1))

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = P . filter f . toList

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pappend :: P a -> P a -> P a
pappend (P p) (P q) = P (p ++ q)

pseq :: [P a] -> Int -> P a
pseq ps n =
    let ps' = concat (replicate n ps)
    in foldr mappend mempty ps'

pser :: [P a] -> Int -> P a
pser p n = ptake n (pseq p inf)

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pswitch1 :: [P a] -> P Int -> P a
pswitch1 ps is =
    if pnull is
    then mempty
    else let (i,j) = psep is
             (l,r) = splitAt (unp i) ps
             p = head r
         in if pnull p
            then pswitch1 ps j
            else let x = phead p
                     ps' = l ++ [ptail p] ++ tail r
                 in x `mappend` pswitch1 ps' j

pchoose :: Enum e => e -> [P a] -> P a
pchoose e p =
    let g = mkStdGen (fromEnum e)
    in mconcat (choosea g (A.listArray (0, length p - 1) p))

fromList :: [a] -> P a
fromList = P

prand :: Enum e => e -> [P a] -> Int -> P a
prand s ps n =
    let g = mkStdGen (fromEnum s)
        qs = choosea g (A.listArray (0, length ps - 1) ps)
    in foldr mappend mempty (take n qs)

prand_b :: (Random a) => StdGen -> P (a,a) -> P a
prand_b g i =
    if pnull i
    then mempty
    else let (h,t) = psep i
             (x,g') = randomR (unp h) g
         in return x `mappend` prand_b g' t

pwhite :: (Enum e,Random a) => e -> P a -> P a -> P a
pwhite n l r =
    let b = pzip (pcycle l) (pcycle r)
        g = mkStdGen (fromEnum n)
    in prand_b g b

pexprand :: (Enum n,Random a,Floating a) => n -> P a -> P a -> P a
pexprand n l r = pzipWith3 to_exprand (pcycle l) (pcycle r) (pwhite n l r)

windex :: (Ord a, Num a) => [a] -> a -> Int
windex w n = fromJust (findIndex (n <) (C.integrate w))

pindex :: P a -> Int -> P a
pindex p n = phead (pdrop n p)

wlookup :: (Ord n,Fractional n) => P a -> [n] -> n -> P a
wlookup x w i = x `pindex` windex w i

pwrand :: (Enum e, Random n, Ord n, Fractional n) => e -> P a -> [n] -> P a
pwrand n x w = join (fmap (wlookup x w) (pwhite n 0 1))

{-
pwrand' :: (Enum n) => n -> P b -> [Double] -> P b
pwrand' n x w = map (wlookup x w) (pwhite n 0 1)
-}

pif :: (a -> Bool) -> P a -> P b -> P b -> P b
pif f = pzipWith3 (\x z y -> if f x then y else z)

-- * Extension (list & pattern)

class Extending f where
    zipWith_c :: (a -> b -> c) -> f a -> f b -> f c

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
