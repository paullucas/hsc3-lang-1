module Sound.SC3.Lang.Pattern.Control where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Sound.SC3.Lang.Math.Pitch
import Sound.SC3.Lang.Pattern.Pattern

pfilter :: (a -> Bool) -> P s a -> P s a
pfilter f p =
    let g x p' = if f x
                 then mappend (return x) (pfilter f p')
                 else pfilter f p'
    in pcontinue p g

plist :: [P s a] -> P s a
plist = foldr mappend mempty

pcons :: a -> P s a -> P s a
pcons = mappend . return

preplicate_ :: Int -> P s a -> P s a
preplicate_ n p | n > 0 = mappend p (preplicate_ (n - 1) p)
                | otherwise = mempty

preplicate :: P s Int -> P s a -> P s a
preplicate n p = n >>= (\x -> preplicate_ x p)

pn :: P s a -> P s Int -> P s a
pn = flip preplicate

pn_ :: P s a -> Int -> P s a
pn_ = flip preplicate_

-- | 'n' initial values at 'p'.
ptake_ :: Int -> P s a -> P s a
ptake_ n p =
    let e = error "ptake_"
    in pzipWith const p (preplicate_ n (return e))

ptake :: P s Int -> P s a -> P s a
ptake n p =
    let e = error "ptake"
    in pzipWith const p (preplicate n (return e))

-- | 'n' initial values at pcycle of 'p'.
prestrict_ :: Int -> P s a -> P s a
prestrict_ n = ptake_ n . pcycle

prestrict :: P s Int -> P s a -> P s a
prestrict n = ptake n . pcycle

pmapMaybe :: (a -> Maybe b) -> P s a -> P s b
pmapMaybe f = fmap fromJust . pfilter isJust . fmap f

preject :: (a -> Bool) -> P s a -> P s a
preject f = pfilter (not . f)

pzipWith3 :: (a -> b -> c -> d) -> P s a -> P s b -> P s c -> P s d
pzipWith3 f p q = (<*>) (pure f <*> p <*> q)

pzip :: P s a -> P s b -> P s (a,b)
pzip = pzipWith (,)

pseries :: (Num a) => a -> a -> Int -> P s a
pseries i s n = plist (unfoldr f (i, n))
    where f (_, 0) = Nothing
          f (j, m) = Just (return j, (j + s, m - 1))

pgeom :: (Num a) => a -> a -> Int -> P s a
pgeom i s n = plist (unfoldr f (i, n))
    where f (_, 0) = Nothing
          f (j, m) = Just (return j, (j * s, m - 1))

pstutter' :: P s Int -> P s a -> P s a
pstutter' n p =
    let f :: Int -> a -> P s a
        f i e = preplicate (return i) (return e)
    in psequence (pzipWith f n p)

pstutter :: P s Int -> P s a -> P s a
pstutter = pstutter' . pcycle

-- | Count false values preceding each true value.
pcountpre :: P s Bool -> P s Int
pcountpre p = pmapMaybe id (pscan f Nothing 0 p)
    where f x e = if e then (0, Just x) else (x + 1, Nothing)

-- | Count false values following each true value.
pcountpost :: P s Bool -> P s Int
pcountpost p = ptail (pmapMaybe id (pscan f (Just Just) 0 p))
    where f x e = if e then (0, Just x) else (x + 1, Nothing)

pclutch' :: P s a -> P s Bool -> P s a
pclutch' p q = pstutter' r p
    where r = fmap (+ 1) (pcountpost q)

pbool :: (Ord a, Num a) => P s a -> P s Bool
pbool = fmap (> 0)

pclutch :: (Num b, Ord b) => P s a -> P s b -> P s a
pclutch p = pclutch' p . pbool

pcollect :: (a -> b) -> P s a -> P s b
pcollect = fmap

pdegreeToKey :: (RealFrac a) => P s a -> P s [a] -> P s a -> P s a
pdegreeToKey = pzipWith3 degree_to_key

pfin :: P s Int -> P s a -> P s a
pfin = ptake

pfin_ :: Int -> P s a -> P s a
pfin_ = ptake_

wrap :: (Ord a, Num a) => a -> a -> a -> a
wrap l r x = if x > r
             then wrap l r (x - (r - l))
             else if x < l
                  then wrap l r (x + (r - l))
                  else x

pwrap :: (Ord a, Num a) => P s a -> P s a -> P s a -> P s a
pwrap x l r = pzipWith3 f x (pcycle l) (pcycle r)
    where f x' l' r' = wrap l' r' x'

-- | Remove successive duplicates.
prsd :: (Eq a) => P s a -> P s a
prsd p = pmapMaybe id (pscan f Nothing Nothing p)
    where f Nothing a = (Just a, Just a)
          f (Just x) a = (Just a, if a == x then Nothing else Just a)

psequence :: P s (P s a) -> P s a
psequence = join

pduple :: (a, a) -> P s a
pduple (x, y) = return x `mappend` return y

pinterleave :: P s a -> P s a -> P s a
pinterleave p = psequence . fmap pduple . pzip p

ptrigger :: P s Bool -> P s a -> P s (Maybe a)
ptrigger p q = join (pzipWith f r q)
    where r = pcountpre p
          f i = mappend (preplicate_ i (return Nothing)) . return . Just

pif :: s -> P s Bool -> P s a -> P s a -> P s a
pif s b p q = pzipWith f p' q'
    where b' = pfix s b
          p' = ptrigger b' p
          q' = ptrigger (fmap not b') q
          f (Just x) Nothing = x
          f Nothing (Just x) = x
          f _ _ = error "pif"

phead :: P s a -> P s a
phead p = pcontinue p (\x _ -> return x)

ptail :: P s a -> P s a
ptail p = pcontinue p (\_ p' -> p')

pdrop :: P s Int -> P s a -> P s a
pdrop n p = n >>= (\x -> if x > 0
                         then pdrop (return (x-1)) (ptail p)
                         else p)

pscanl :: (a -> y -> a) -> a -> P s y -> P s a
pscanl f i p = pcons i (pscan g Nothing i p)
    where g x y = let r = f x y in (r, r)
