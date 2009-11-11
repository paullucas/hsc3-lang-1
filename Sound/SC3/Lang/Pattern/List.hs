module Sound.SC3.Lang.Pattern.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.HashTable as H
import qualified Data.List as L
import qualified Data.Monoid as M
import qualified Data.Traversable as T
import qualified Sound.SC3.Lang.Collection.Collection as S
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as S
import qualified Sound.SC3.Lang.Math.Pitch as S
import qualified System.Random as R

data P a = P { unP :: [a] }

pempty :: P a
pempty = P []

pnull :: P a -> Bool
pnull = L.null . unP

pcons :: a -> P a -> P a
pcons x = P . (x:) . unP

pappend :: P a -> P a -> P a
pappend p q = P (unP p ++ unP q)

-- inspects data
phead :: P a -> Maybe a
phead (P []) = Nothing
phead (P (x:_)) = Just x

ptail :: P a -> P a
ptail =
    let f [] = []
        f (_:xs) = xs
    in P . f . unP

pdrop :: P Int -> P a -> P a
pdrop n =
    case phead n of
      Nothing -> error "pdrop"
      Just n' -> P . L.drop n' . unP

pconcat :: P (P a) -> P a
pconcat p =
    if pnull p
    then pempty
    else case phead p of
           Just x -> x `pappend` (pconcat (ptail p))
           Nothing -> pempty

ptake :: P Int -> P a -> P a
ptake n =
    case phead n of
      Just n' -> P . L.take n' . unP
      Nothing -> error "ptake: empty length pattern"

pfin :: P Int -> P a -> P a
pfin = ptake

pconcatMap :: (b -> P a) -> P b -> P a
pconcatMap f = pconcat . fmap f

pfoldr :: (a -> b -> b) -> b -> P a -> b
pfoldr f x = L.foldr f x . unP

instance F.Foldable P where
    foldr = pfoldr

pcycle :: P a -> P a
pcycle = P . L.cycle . unP

prepeat :: a -> P a
prepeat = P . L.repeat

papply :: P (a -> b) -> P a -> P b
papply (P f) (P x) = P (f A.<*> x)

instance Functor P where
    fmap f = P . fmap f . unP

instance A.Applicative P where
    pure = M.return
    (<*>) = M.ap

instance (Show a) => Show (P a) where
    show = show . unP

instance (Eq a) => Eq (P a) where
    (P p) == (P q) = p == q

instance Monad P where
    m >>= f = pconcatMap f m
    return x = P [x]
    fail _ = P []

instance M.MonadPlus P where
    mzero = pempty
    mplus = pappend

instance M.Monoid (P a) where
    mempty = pempty
    mappend = pappend

pzip :: P a -> P b -> P (a, b)
pzip (P p) (P q) = P (zip p q)

pzip3 :: P a -> P b -> P c -> P (a, b, c)
pzip3 (P p) (P q) (P r) = P (zip3 p q r)

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f (P p) (P q) = P (L.zipWith f p q)

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f (P p) (P q) (P r) = P (L.zipWith3 f p q r)

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

instance T.Traversable P where
    traverse f = let cons_f x ys = pcons A.<$> f x A.<*> ys
                 in pfoldr cons_f (A.pure pempty)

instance A.Alternative P where
    empty = pempty
    (<|>) = pappend

-- * Basic constructors

pinf :: P Int
pinf = return 83886028 -- 2 ^^ 23

-- * List

pseq :: [P a] -> P Int -> P a
pseq ps n =
    case phead n of
      Just n' -> let ps' = concat (replicate n' ps)
                 in L.foldr pappend pempty ps'
      Nothing -> error "pseq: empty repeat pattern"

pser :: [P a] -> P Int -> P a
pser ps n = ptake n (pseq ps pinf)

-- * Random

pnoise :: (R.Random a) => String -> P a
pnoise s =
    let g = R.mkStdGen (fromIntegral (H.hashString s))
    in P (R.randoms g)

prand_b :: (R.Random a) => R.StdGen -> P (a,a) -> P a
prand_b g b =
    case phead b of
      Nothing -> pempty
      Just b' -> let (x, g') = R.randomR b' g
                 in pcons x (prand_b g' (ptail b))

pwhite :: (R.Random a) => String -> P a -> P  a -> P a
pwhite s l r =
    let b = pzip (pcycle l) (pcycle r)
        g = R.mkStdGen (fromIntegral (H.hashString s))
    in prand_b g b

choosea :: R.StdGen -> A.Array Int a -> [a]
choosea g r = 
    let (i, g') = R.randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

pchoose :: String -> P a -> P a
pchoose s (P p) = 
    let g = R.mkStdGen (fromIntegral (H.hashString s))
    in P (choosea g (A.listArray (0, length p - 1) p))

prand :: String -> [P a] -> P Int -> P a
prand s ps n = 
    case phead n of
      Nothing -> error "prand"
      Just n' -> let g = R.mkStdGen (fromIntegral (H.hashString s))
                     qs = choosea g (A.listArray (0, length ps - 1) ps)
                 in L.foldr pappend pempty (take n' qs)

-- * Extend

pzipWith_c :: (a -> b -> c) -> P a -> P b -> P c
pzipWith_c f p = pzipWith f p . pcycle

(+.) :: Num a => P a -> P a -> P a
(+.) = pzipWith_c (+)

(*.) :: Num a => P a -> P a -> P a
(*.) = pzipWith_c (*)

(/.) :: Fractional a => P a -> P a -> P a
(/.) = pzipWith_c (/)

(-.) :: Num a => P a -> P a -> P a
(-.) = pzipWith_c (-)

-- * Control

bool :: (Functor f, Ord a, Num a) => f a -> f Bool
bool = fmap (> 0)

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = bool

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = P . L.filter f . unP

preject :: (a -> Bool) -> P a -> P a
preject f =
    let g i _ = f i
    in P . S.reject g . unP

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (S.series n i s)

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = P (S.geom n i s)

stutter :: [Int] -> [a] -> [a]
stutter [] _ = []
stutter _ [] = []
stutter (n:ns) (p:ps) = replicate n p ++ stutter ns ps

pstutter :: P Int -> P a -> P a
pstutter (P n) (P p) = P (stutter n p)

-- | Count false values preceding each true value.
countpre :: [Bool] -> [Int]
countpre =
    let f i [] = if i == 0 then [] else [i]
        f i (x:xs) = if x 
                     then i : f 0 xs
                     else f (i + 1) xs
    in f 0

pcountpre :: P Bool -> P Int
pcountpre = P . countpre . unP

-- | Remove successive duplicates.
rsd :: (Eq a) => [a] -> [a]
rsd =
    let f _ [] = []
        f Nothing (x:xs) = x : f (Just x) xs
        f (Just y) (x:xs) = if x == y
                            then f (Just x) xs
                            else x : f (Just x) xs
    in f Nothing

prsd :: (Eq a) => P a -> P a
prsd = P . rsd . unP

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pswitch1 :: [P a] -> P Int -> P a
pswitch1 ps i =
    case phead i of
      Nothing -> pempty
      Just i' -> let (l, r) = splitAt i' ps
                     (p:_) = r
                     x = phead p
                     j = ptail i
                 in case x of
                      Nothing -> pswitch1 ps j
                      Just x' -> let ps' = l ++ [ptail p] ++ tail r
                                 in x' `pcons` pswitch1 ps' j

-- | Count false values following each true value.
countpost :: [Bool] -> [Int]
countpost =
    let f i [] = [i]
        f i (x:xs) = if not x
                     then f (i + 1) xs
                     else i : f 0 xs
    in tail . f 0

pcountpost :: P Bool -> P Int
pcountpost = P . countpost . unP

clutch :: [a] -> [Bool] -> [a]
clutch p q =
    let r = fmap (+ 1) (countpost q)
    in stutter r p

pclutch :: P a -> P Bool -> P a
pclutch (P x) (P c) = P (clutch x c)

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 S.degree_to_key

interleave :: [a] -> [a] -> [a]
interleave p [] = p
interleave [] q = q
interleave (p:ps) (q:qs) = p : q : interleave ps qs

pinterleave :: P a -> P a -> P a
pinterleave (P p) (P q) = P (interleave p q)

pn :: P a -> P Int -> P a
pn (P p) n =
    let f 0 _ = []
        f i xs = xs ++ f (i - 1) xs
    in case phead n of
         Nothing -> error "preplicate"
         Just x -> P (f x p)

trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in concat (zipWith f r q)

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger (P p) (P q) = P (trigger p q)
