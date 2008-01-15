module Sound.SC3.Lang.Pattern.Control where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Sound.SC3.Lang.Pattern.Pattern
import Sound.SC3.Lang.Pattern.Extend

plist :: [P a] -> P a
plist = foldr mappend mempty

paccf :: (x -> a -> (x, y)) -> (x -> y) -> x -> P a -> P y
paccf f g = pacc h
    where h x Nothing = (undefined, g x)
          h x (Just a) = f x a

pcycle :: P a -> P a
pcycle x = mappend x (pcycle x)

pcons :: a -> P a -> P a
pcons = mappend . return

prepeat :: a -> P a
prepeat = pcycle . return

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

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p = (<*>) (pure f <*> p)

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

pstutter :: P Int -> P a -> P a
pstutter n p = pzipWithL (\i e -> preplicate (return i) (return e)) n p >>= id

pstutter' :: P Int -> P a -> P a
pstutter' n p = pzipWith (\i e -> preplicate (return i) (return e)) n p >>= id

-- | Count false values preceding each true value. 
pcountpre :: P Bool -> P Int
pcountpre p = pmapMaybe id (paccf f (const Nothing) 0 p)
    where f x e = if e then (0, Just x) else (x + 1, Nothing)

-- | Count false values following each true value. 
pcountpost :: P Bool -> P Int
pcountpost p = pmapMaybe id (paccf f Just 0 p)
    where f x e = if e then (0, Just x) else (x + 1, Nothing)

pclutch' :: P a -> P Bool -> P a
pclutch' p q = pstutter' r p
    where r = ptail (fmap (+ 1) (pcountpost q))

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q = pzipWith f r q >>= id
    where r = pcountpre p
          f i e = mappend (preplicate_ i (return Nothing)) (return (Just e))

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fmap (> 0)

pclutch :: (Num b, Ord b) => P a -> P b -> P a
pclutch p q = pclutch' p (pbool q)

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

degree_to_key :: (RealFrac a) => a -> [a] -> a -> a
degree_to_key d s n = (n * fromIntegral (d' `div` l)) + (s !! (d' `mod` l)) + a
    where l = length s
          d' = round d
          a = (d - fromIntegral d') * 10.0 * (n / 12.0)

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3L degree_to_key

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
pwrap = pzipWith3L (\x l r -> wrap l r x)

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd p = pmapMaybe id (paccf f (const Nothing) Nothing p)
    where f Nothing a = (Just a, Just a)
          f (Just x) a = (Just a, if a == x then Nothing else Just a)

pinterleave' :: P a -> P a -> P a
pinterleave' p q = pzipWith (\x y -> mappend (return x) (return y)) p q >>= id

pinterleave :: P a -> P a -> P a
pinterleave p q = pzipWithL (\x y -> mappend (return x) (return y)) p q >>= id

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
ptail p = pmapMaybe f (pzip p q)
    where q = pcons False (prepeat True)
          f (x, y) = if y then Just x else Nothing
