module Sound.SC3.Lang.Pattern.Control where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Sound.SC3.Lang.Math.Pitch
import Sound.SC3.Lang.Pattern.Pattern

pfilter :: (a -> Bool) -> P a -> P a
pfilter f p =
    let g x p' = if f x
                 then mappend (return x) (pfilter f p')
                 else pfilter f p'
    in pcontinue p g

plist :: [P a] -> P a
plist = foldr mappend mempty

pcons :: a -> P a -> P a
pcons = mappend . return

preplicate_ :: Int -> P a -> P a
preplicate_ n p | n > 0 = mappend p (preplicate_ (n - 1) p)
                | otherwise = mempty

preplicate :: P Int -> P a -> P a
preplicate n p = n >>= (\x -> preplicate_ x p)

pn :: P a -> P Int -> P a
pn = flip preplicate

pn_ :: P a -> Int -> P a
pn_ = flip preplicate_

-- | 'n' initial values at 'p'.
ptake_ :: Int -> P a -> P a
ptake_ n p =
    let e = error "ptake_"
    in pzipWith const p (preplicate_ n (return e))

ptake :: P Int -> P a -> P a
ptake n p =
    let e = error "ptake"
    in pzipWith const p (preplicate n (return e))

-- | 'n' initial values at pcycle of 'p'.
prestrict_ :: Int -> P a -> P a
prestrict_ n = ptake_ n . pcycle

prestrict :: P Int -> P a -> P a
prestrict n = ptake n . pcycle

pmapMaybe :: (a -> Maybe b) -> P a -> P b
pmapMaybe f = fmap fromJust . pfilter isJust . fmap f

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f p q = (<*>) (pure f <*> p <*> q)

pzip :: P a -> P b -> P (a,b)
pzip = pzipWith (,)

pzip3 :: P a -> P b -> P c -> P (a,b,c)
pzip3 = pzipWith3 (,,)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n =
    let f (_, 0) = Nothing
        f (j, m) = Just (return j, (j + s, m - 1))
    in plist (unfoldr f (i, n))

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n =
    let f (_, 0) = Nothing
        f (j, m) = Just (return j, (j * s, m - 1))
    in plist (unfoldr f (i, n))

pstutter' :: P Int -> P a -> P a
pstutter' n p =
    let f :: Int -> a -> P a
        f i e = preplicate (return i) (return e)
    in psequence (pzipWith f n p)

pstutter :: P Int -> P a -> P a
pstutter = pstutter' . pcycle

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre p =
    let f x e = if e then (0, Just x) else (x + 1, Nothing)
    in pmapMaybe id (pscan f Nothing 0 p)

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost p =
    let f x e = if e then (0, Just x) else (x + 1, Nothing)
    in ptail (pmapMaybe id (pscan f (Just Just) 0 p))

pclutch' :: P a -> P Bool -> P a
pclutch' p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter' r p

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fmap (> 0)

pclutch :: (Num b, Ord b) => P a -> P b -> P a
pclutch p = pclutch' p . pbool

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 degree_to_key

pfin :: P Int -> P a -> P a
pfin = ptake

pfin_ :: Int -> P a -> P a
pfin_ = ptake_

wrap :: (Ord a, Num a) => a -> a -> a -> a
wrap l r x = if x > r
             then wrap l r (x - (r - l))
             else if x < l
                  then wrap l r (x + (r - l))
                  else x

pwrap :: (Ord a, Num a) => P a -> P a -> P a -> P a
pwrap x l r =
    let f x' l' r' = wrap l' r' x'
    in pzipWith3 f x (pcycle l) (pcycle r)

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd p =
    let f Nothing a = (Just a, Just a)
        f (Just x) a = (Just a, if a == x then Nothing else Just a)
    in pmapMaybe id (pscan f Nothing Nothing p)

psequence :: P (P a) -> P a
psequence = join

pduple :: (a, a) -> P a
pduple (x, y) = return x `mappend` return y

pinterleave :: P a -> P a -> P a
pinterleave p = psequence . fmap pduple . pzip p

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i = mappend (preplicate_ i (return Nothing)) . return . Just
    in join (pzipWith f r q)

pif :: P Bool -> P a -> P a -> P a
pif b p q =
    let f (x, y) True = ((ptail x, y), phead x)
        f (x, y) False = ((x, ptail y), phead y)
    in psequence (pscan f Nothing (p,q) b)

phead :: P a -> P a
phead p = pcontinue p (\x _ -> return x)

ptail :: P a -> P a
ptail p = pcontinue p (\_ p' -> p')

pdrop :: P Int -> P a -> P a
pdrop n p = n >>= (\x -> if x > 0
                         then pdrop (return (x-1)) (ptail p)
                         else p)

pscanl :: (a -> y -> a) -> a -> P y -> P a
pscanl f i p =
    let g x y = let r = f x y in (r, r)
    in pcons i (pscan g Nothing i p)
