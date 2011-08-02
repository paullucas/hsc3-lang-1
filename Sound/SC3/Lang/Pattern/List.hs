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

-- * P type

newtype P a = P {toList :: [a]}
    deriving (Eq,Functor,Monoid,Foldable,Traversable,Monad,Show)

instance Applicative P where
    pure = P . repeat
    P f <*> P e = P (map (\(f',e') -> f' e') (zip f e))

pnil :: P a
pnil = P []

pnull :: P a -> Bool
pnull = null . toList

unp :: P a -> a
unp (P p) =
    case p of
      (e:_) -> e
      _ -> error "unp"

psep :: P a -> (P a,P a)
psep (P p) =
    case p of
      [] -> (P [],P [])
      [e] -> (P [e],P [])
      (e:p') -> (P [e],P p')

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = P . filter f . toList

fromList :: [a] -> P a
fromList = P

-- * Common (to List & Parallel)

instance (Num a) => Num (P a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional (P a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = return . fromRational

ppure :: a -> P a
ppure = pure

prepeat :: a -> P a
prepeat = pure

papply :: P (a -> b) -> P a -> P b
papply = (<*>)

pmap :: (a -> b) -> P a -> P b
pmap = fmap

preturn :: a -> P a
preturn = return

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith = liftA2

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 = liftA3

pzip :: P a -> P b -> P (a, b)
pzip = pzipWith (,)

-- | A very large positive integer
inf :: Int
inf = 83886028

-- | Choose an element of an array at random
choosea :: StdGen -> A.Array Int a -> [a]
choosea g r =
    let (i, g') = randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

rrand :: (Random a, Enum e) => e -> a -> a -> a
rrand e a b =
    let g = mkStdGen (fromEnum e)
    in fst (randomR (a,b) g)

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

fbool :: (Functor f, Ord a, Num a) => f a -> f Bool
fbool = fmap (> 0)

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fbool

phead :: P a -> P a
phead = fst . psep

ptail :: P a -> P a
ptail = snd . psep

ptake :: Int -> P a -> P a
ptake n p =
    if n > 0
    then let (h,t) = psep p
         in h `mappend` ptake (n - 1) t
    else mempty

pdrop :: Int -> P a -> P a
pdrop n p = if n > 0 then pdrop (n - 1) (ptail p) else p

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost =
    let f i p = if pnull p
                then return i
                else let (x,xs) = psep p
                     in if not (unp x)
                        then f (i + 1) xs
                        else return i `mappend` f 0 xs
    in ptail . f 0

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre =
    let f i p = if pnull p
                then if i == 0 then mempty else return i
                else let (x,xs) = psep p
                     in if unp x
                        then return i `mappend` f 0 xs
                        else f (i + 1) xs
    in f 0

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing `mappend` return (Just x)
    in join (pzipWith f r q)

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 P.degree_to_key

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd =
    let f i p = let (h,t) = psep p
                in if pnull h
                   then mempty
                   else let t' = f (Just h) t
                        in case i of
                          Nothing -> h `mappend` t'
                          Just j -> if j == h then t' else h `mappend` t'
    in f Nothing

pser :: [P a] -> Int -> P a
pser ps n = ptake n (pseq ps inf)

pcycle :: P a -> P a
pcycle a = a `mappend` pcycle a

pstutter :: P Int -> P a -> P a
pstutter ns = join . pzipWith preplicate (pcycle ns)

preplicate :: Int -> a -> P a
preplicate n = fromList . replicate n

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = fromList (C.geom n i s)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = fromList (C.series n i s)

pinterleave :: P a -> P a -> P a
pinterleave p q =
    let ((a,p'),(b,q')) = (psep p,psep q)
    in if pnull b
       then p
       else if pnull a
            then q
            else a `mappend` b `mappend` pinterleave p' q'

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pn :: P a -> Int -> P a
pn p n = if n == 0 then mempty else p `mappend` (pn p (n - 1))

pseq :: [P a] -> Int -> P a
pseq ps n =
    let ps' = concat (replicate n ps)
    in foldr mappend mempty ps'

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

pif :: (a -> Bool) -> P a -> P b -> P b -> P b
pif f = pzipWith3 (\x z y -> if f x then y else z)

psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p = (ptake n p,pdrop n p)

pcons :: a -> P a -> P a
pcons e p = return e `mappend` p

-- * Extension (list & pattern)

class Extending f where
    zipWith_c :: (a -> b -> c) -> f a -> f b -> f c

pzipWith_c :: (a -> b -> c) -> P a -> P b -> P c
pzipWith_c f ip iq =
    let go l r p q =
            let l' = l || pnull p
                r' = r || pnull q
                p' = if pnull p then ip else p
                q' = if pnull q then iq else q
            in if l' && r
               then pnil
               else let (ph,pt) = psep p'
                    in if r' && l
                       then pnil
                       else let (qh,qt) = psep q'
                            in f (unp ph) (unp qh) `pcons` go l' r' pt qt
    in go False False ip iq

instance Extending P where
    zipWith_c = pzipWith_c

(+.) :: (Extending f,Num a) => f a -> f a -> f a
(+.) = zipWith_c (+)

(*.) :: (Extending f,Num a) => f a -> f a -> f a
(*.) = zipWith_c (*)

(/.) :: (Extending f,Fractional a) => f a -> f a -> f a
(/.) = zipWith_c (/)

(-.) :: (Extending f,Num a) => f a -> f a -> f a
(-.) = zipWith_c (-)
