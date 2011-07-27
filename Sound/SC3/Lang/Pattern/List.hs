module Sound.SC3.Lang.Pattern.List where

import Control.Monad
import qualified Data.Array as A
import Data.Bool
import Data.Foldable
import Data.List hiding (concat,foldr,find)
import Data.Maybe
import Data.Monoid
import Prelude hiding (concat,foldr)
import Sound.SC3.Identifier
import qualified Sound.SC3.Lang.Collection.Collection as S
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as S
import qualified Sound.SC3.Lang.Math.Pitch as S
import System.Random

-- * List instances

instance (Num a) => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional [a] where
    (/) = zipWith (/)
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

rrand :: (Random a1, ID a) => a -> a1 -> a1 -> a1
rrand n a b =
    let g = mkStdGen (resolveID n)
    in fst (randomR (a,b) g)

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

-- * Pattern functions

pbool :: (Functor f, Ord a, Num a) => f a -> f Bool
pbool = fmap (> 0)

ptail :: [a] -> [a]
ptail x = if null x then [] else tail x

-- | Count false values following each true value.
pcountpost :: [Bool] -> [Int]
pcountpost =
    let f i [] = [i]
        f i (x:xs) = if not x
                     then f (i + 1) xs
                     else i : f 0 xs
    in tail . f 0

pclutch :: [a] -> [Bool] -> [a]
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

pinterleave :: [a] -> [a] -> [a]
pinterleave xs ys =
    case (xs,ys) of
      (_,[]) -> xs
      ([],_) -> ys
      (p:ps,q:qs) -> p : q : pinterleave ps qs

pdegreeToKey :: (RealFrac a) => [a] -> [[a]] -> [a] -> [a]
pdegreeToKey = zipWith3 S.degree_to_key

-- | Remove successive duplicates.
prsd :: (Eq a) => [a] -> [a]
prsd =
    let f _ [] = []
        f Nothing (x:xs) = x : f (Just x) xs
        f (Just y) (x:xs) = if x == y
                            then f (Just x) xs
                            else x : f (Just x) xs
    in f Nothing

pstutter :: [Int] -> [a] -> [a]
pstutter ns = concat . zipWith replicate (cycle ns)

-- | Count false values preceding each true value.
pcountpre :: [Bool] -> [Int]
pcountpre =
    let f i [] = if i == 0 then [] else [i]
        f i (x:xs) = if x
                     then i : f 0 xs
                     else f (i + 1) xs
    in f 0

ptrigger :: [Bool] -> [a] -> [Maybe a]
ptrigger p q =
    let r = pcountpre p
        f i x = replicate i Nothing ++ [Just x]
    in concat (zipWith f r q)

pgeom :: (Num a) => a -> a -> Int -> [a]
pgeom i s n = S.geom n i s

pseries :: (Num a) => a -> a -> Int -> [a]
pseries i s n = S.series n i s

pn :: [a] -> [Int] -> [a]
pn xs is =
    case is of
      [] -> error "pn"
      i:_ -> concat (take i (repeat xs))

preject :: (a -> Bool) -> [a] -> [a]
preject f =
    let g i _ = f i
    in S.reject g

pseq :: [[a]] -> Int -> [a]
pseq ps n =
    let ps' = concat (replicate n ps)
    in foldr mappend mempty ps'

pser :: [[a]] -> Int -> [a]
pser ps n = take n (pseq ps inf)

pswitch :: [[a]] -> [Int] -> [a]
pswitch l i = i >>= (l !!)

pswitch1 :: [[a]] -> [Int] -> [a]
pswitch1 ps is =
    case is of
      [] -> []
      i:_ -> let (l,r) = splitAt i ps
                 (p:_) = r
                 j = tail is
             in case p of
                  [] -> pswitch1 ps j
                  x:_ -> let ps' = l ++ [tail p] ++ tail r
                         in x : pswitch1 ps' j

pchoose :: ID n => n -> [a] -> [a]
pchoose n p =
    let g = mkStdGen (resolveID n)
    in choosea g (A.listArray (0, length p - 1) p)

prand :: ID n => n -> [[a]] -> Int -> [a]
prand s ps n =
    let g = mkStdGen (resolveID s)
        qs = choosea g (A.listArray (0, length ps - 1) ps)
    in foldr (++) [] (take n qs)

prand_b :: (Random a) => StdGen -> [(a,a)] -> [a]
prand_b g b =
    case headM b of
      Nothing -> mempty
      Just b' -> let (x, g') = randomR b' g
                 in x : (prand_b g' (tail b))

pwhite :: (ID n,Random a) => n -> [a] -> [a] -> [a]
pwhite n l r =
    let b = zip (cycle l) (cycle r)
        g = mkStdGen (resolveID n)
    in prand_b g b

pexprand :: (ID n,Random a,Floating a) => n -> [a] -> [a] -> [a]
pexprand n l r = zipWith3 to_exprand (cycle l) (cycle r) (pwhite n l r)

wlookup :: (Ord n,Fractional n) => [a] -> [n] -> n -> a
wlookup x w i =
    let f (j,_) = i < j
    in snd (fromJust (find f (zip (scanl1 (+) w) x)))

pwrand :: (ID n, Random a, Ord a, Fractional a) => n -> [b] -> [a] -> [b]
pwrand n x w = map (wlookup x w) (pwhite n 0 1)

pwrand' :: (ID n) => n -> [b] -> [Double] -> [b]
pwrand' n x w = map (wlookup x w) (pwhite n 0 1)

pif :: (a -> Bool) -> [a] -> [d] -> [d] -> [d]
pif f = zipWith3 (\x z y -> if f x then y else z)

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
