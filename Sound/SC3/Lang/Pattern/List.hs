module Sound.SC3.Lang.Pattern.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Monoid as M
import qualified Data.Traversable as T
import Sound.SC3.Identifier
import qualified Sound.SC3.Lang.Collection.Collection as S
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as S
import qualified Sound.SC3.Lang.Math.Pitch as S
import qualified System.Random as R

data P a = P { unP :: [a] }

-- * Instances

instance A.Alternative P where
    empty = pempty
    (<|>) = pappend

instance A.Applicative P where
    pure = M.return
    (<*>) = M.ap

instance F.Foldable P where
    foldr = pfoldr

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

instance M.MonadPlus P where
    mzero = pempty
    mplus = pappend

instance M.Monoid (P a) where
    mempty = pempty
    mappend = pappend

instance (Num a) => Num (P a) where
    (+) = pzipWith (+)
    (-) = pzipWith (-)
    (*) = pzipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Show a) => Show (P a) where
    show = show . unP

instance T.Traversable P where
    traverse f = let cons_f x ys = pcons A.<$> f x A.<*> ys
                 in pfoldr cons_f (A.pure pempty)

-- * Basic constructors

pinf :: P Int
pinf = return 83886028 -- 2 ^^ 23

-- * List functions

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
papply (P f) (P x) = P (f A.<*> x)

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = bool

pclutch :: P a -> P Bool -> P a
pclutch (P x) (P c) = P (clutch x c)

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
    else case phead p of
           Nothing -> pempty
           Just x -> x `pappend` (pconcat (ptail p))

pconcatMap :: (b -> P a) -> P b -> P a
pconcatMap f = pconcat . fmap f

pcons :: a -> P a -> P a
pcons x = plift (x:)

pcycle :: P a -> P a
pcycle = plift L.cycle

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 S.degree_to_key

pdrop :: P Int -> P a -> P a
pdrop n =
    case phead n of
      Nothing -> error "pdrop"
      Just n' -> plift (L.drop n')

pempty :: P a
pempty = P []

pfilter :: (a -> Bool) -> P a -> P a
pfilter = plift . L.filter

pfin :: P Int -> P a -> P a
pfin = ptake

pfoldr :: (a -> b -> b) -> b -> P a -> b
pfoldr f x = L.foldr f x . unP

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = P (S.geom n i s)

phead :: P a -> Maybe a
phead (P []) = Nothing
phead (P (x:_)) = Just x

pinterleave :: P a -> P a -> P a
pinterleave (P p) (P q) = P (interleave p q)

pn :: P a -> P Int -> P a
pn (P p) n =
    let f 0 _ = []
        f i xs = xs ++ f (i - 1) xs
    in case phead n of
         Nothing -> error "preplicate"
         Just x -> P (f x p)

pnull :: P a -> Bool
pnull = L.null . unP

prepeat :: a -> P a
prepeat = P . L.repeat

preject :: (a -> Bool) -> P a -> P a
preject f =
    let g i _ = f i
    in plift (S.reject g)

prsd :: (Eq a) => P a -> P a
prsd = plift rsd

pseq :: [P a] -> P Int -> P a
pseq ps n =
    case phead n of
      Nothing -> error "pseq: empty repeat pattern"
      Just n' -> let ps' = concat (replicate n' ps)
                 in L.foldr pappend pempty ps'

pser :: [P a] -> P Int -> P a
pser ps n = ptake n (pseq ps pinf)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (S.series n i s)

pstutter :: P Int -> P a -> P a
pstutter (P n) (P p) = P (stutter n p)

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

ptail :: P a -> P a
ptail = plift (\xs -> if null xs then [] else tail xs)

ptake :: P Int -> P a -> P a
ptake n =
    case phead n of
      Nothing -> error "ptake: empty length pattern"
      Just n' -> plift (L.take n')

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger (P p) (P q) = P (trigger p q)

pzip :: P a -> P b -> P (a, b)
pzip (P p) (P q) = P (zip p q)

pzip3 :: P a -> P b -> P c -> P (a, b, c)
pzip3 (P p) (P q) (P r) = P (zip3 p q r)

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f (P p) (P q) = P (L.zipWith f p q)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f (P p) (P q) (P r) = P (L.zipWith3 f p q r)

-- * Random patterns

choosea :: R.StdGen -> A.Array Int a -> [a]
choosea g r =
    let (i, g') = R.randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

pchoose :: ID n => n -> P a -> P a
pchoose n (P p) =
    let g = R.mkStdGen (resolveID n)
    in P (choosea g (A.listArray (0, length p - 1) p))

pnoise :: ID n => (R.Random a) => n -> P a
pnoise n =
    let g = R.mkStdGen (resolveID n)
    in P (R.randoms g)

prand :: ID n => n -> [P a] -> P Int -> P a
prand s ps n =
    case phead n of
      Nothing -> error "prand"
      Just n' -> let g = R.mkStdGen (resolveID s)
                     qs = choosea g (A.listArray (0, length ps - 1) ps)
                 in L.foldr pappend pempty (take n' qs)

prand_b :: (R.Random a) => R.StdGen -> P (a,a) -> P a
prand_b g b =
    case phead b of
      Nothing -> pempty
      Just b' -> let (x, g') = R.randomR b' g
                 in pcons x (prand_b g' (ptail b))

pwhite :: (ID n,R.Random a) => n -> P a -> P  a -> P a
pwhite n l r =
    let b = pzip (pcycle l) (pcycle r)
        g = R.mkStdGen (resolveID n)
    in prand_b g b

-- * Extension

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
