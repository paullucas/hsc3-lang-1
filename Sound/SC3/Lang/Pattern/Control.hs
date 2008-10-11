module Sound.SC3.Lang.Pattern.Control where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Sound.SC3.Lang.Math.Pitch
import Sound.SC3.Lang.Pattern.Pattern

pfilter :: (a -> Bool) -> P a -> P a
pfilter f p = pcontinue p (\x p' -> if f x 
                                    then mappend (return x) (pfilter f p')
                                    else pfilter f p')

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
ptake_ n p = pzipWith const p (preplicate_ n (return undefined))

ptake :: P Int -> P a -> P a
ptake n p = pzipWith const p (preplicate n (return undefined))

-- | 'n' initial values at pcycle of 'p'.
prestrict_ :: Int -> P a -> P a
prestrict_ n p = ptake_ n (pcycle p)

prestrict :: P Int -> P a -> P a
prestrict n p = ptake n (pcycle p)

pmapMaybe :: (a -> Maybe b) -> P a -> P b
pmapMaybe f p = fmap fromJust (pfilter isJust (fmap f p))

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f p q = (<*>) (pure f <*> p <*> q)

pzip :: P a -> P b -> P (a,b)
pzip = pzipWith (,)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = plist (unfoldr f (i, n))
    where f (_, 0) = Nothing
          f (j, m) = Just (return j, (j + s, m - 1))

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = plist (unfoldr f (i, n))
    where f (_, 0) = Nothing
          f (j, m) = Just (return j, (j * s, m - 1))

pstutter' :: P Int -> P a -> P a
pstutter' n p = psequence (pzipWith f n p)
    where f i e = preplicate (return i) (return e)

pstutter :: P Int -> P a -> P a
pstutter n = pstutter' (pcycle n)

-- | Count false values preceding each true value. 
pcountpre :: P Bool -> P Int
pcountpre p = pmapMaybe id (pacc f Nothing 0 p)
    where f x e = if e then (0, Just x) else (x + 1, Nothing)

-- | Count false values following each true value. 
pcountpost :: P Bool -> P Int
pcountpost p = ptail (pmapMaybe id (pacc f (Just Just) 0 p))
    where f x e = if e then (0, Just x) else (x + 1, Nothing)

pclutch' :: P a -> P Bool -> P a
pclutch' p q = pstutter' r p
    where r = fmap (+ 1) (pcountpost q)

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fmap (> 0)

pclutch :: (Num b, Ord b) => P a -> P b -> P a
pclutch p q = pclutch' p (pbool q)

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
pwrap x l r = pzipWith3 f x (pcycle l) (pcycle r)
    where f x' l' r' = wrap l' r' x'

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd p = pmapMaybe id (pacc f Nothing Nothing p)
    where f Nothing a = (Just a, Just a)
          f (Just x) a = (Just a, if a == x then Nothing else Just a)

psequence :: P (P a) -> P a
psequence = join

pduple :: (a, a) -> P a
pduple (x, y) = return x `mappend` return y

pinterleave :: P a -> P a -> P a
pinterleave p q = psequence (fmap pduple (pzip p q))

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q = join (pzipWith f r q)
    where r = pcountpre p
          f i e = mappend (preplicate_ i (return Nothing)) (return (Just e))

pif :: Int -> P Bool -> P a -> P a -> P a
pif s b p q = pzipWith f p' q'
    where b' = pfix s b
          p' = ptrigger b' p
          q' = ptrigger (fmap not b') q
          f (Just x) Nothing = x
          f Nothing (Just x) = x
          f _ _ = undefined

pif' :: P Bool -> P a -> P a -> P a
pif' = pif 0

ptail :: P a -> P a
ptail p = pcontinue p (\_ p' -> p')

pdrop :: P Int -> P a -> P a
pdrop n p = n >>= (\x -> if x > 0 
                         then pdrop (return (x-1)) (ptail p)
                         else p)

pscanl :: (a -> y -> a) -> a -> P y -> P a
pscanl f i p = pcons i (pacc g Nothing i p)
    where g x y = let r = f x y in (r, r) 
