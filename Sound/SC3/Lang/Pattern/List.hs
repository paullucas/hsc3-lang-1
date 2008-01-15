module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Sound.SC3.Lang.Pattern.Pattern
import Sound.SC3.Lang.Pattern.Extend

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

plist :: [P a] -> P a
plist [] = mempty
plist (x:xs) = mappend x (plist xs)

preplicate_ :: Int -> P a -> P a
preplicate_ 0 _ = mempty
preplicate_ n p = mappend p (preplicate_ (n - 1) p)

preplicate :: P Int -> P a -> P a
preplicate n p = n >>= (\x -> plist (replicate x p))

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

-- | 'n' 
pseq_ :: [P a] -> Int -> P a
pseq_ l n = plist (concat (replicate n l))

pseq :: [P a] -> P Int -> P a
pseq l n = n >>= (\x -> plist (concat (replicate x l)))

-- | 'n' values from the infinite cycle of the streams at l.
pser_ :: [P a] -> Int -> P a
pser_ l n = prestrict_ n (plist l)

pser :: [P a] -> P Int -> P a
pser l n = prestrict n (plist l)

pmapMaybe :: (a -> Maybe b) -> P a -> P b
pmapMaybe f p = fmap fromJust (pfilter isJust (fmap f p))

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p = (<*>) (pure f <*> p)

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

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pstutter :: P Int -> P a -> P a
pstutter n p = pzipWithL (\i e -> preplicate (return i) (return e)) n p >>= id

pstutter' :: P Int -> P a -> P a
pstutter' n p = pzipWith (\i e -> preplicate (return i) (return e)) n p >>= id

pclutch :: (Num b, Ord b) => P a -> P b -> P a
pclutch p q = pstutter' r p
    where f x e = if e then (1, Just x) else (x + 1, Nothing)
          r = pmapMaybe id (paccf f Just 0 (pbool q))

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fmap (> 0)

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

ppatlace :: [P a] -> P Int -> P a
ppatlace = undefined

pinterleave' :: P a -> P a -> P a
pinterleave' p q = pzipWith (\x y -> mappend (return x) (return y)) p q >>= id

pinterleave :: P a -> P a -> P a
pinterleave p q = pzipWithL (\x y -> mappend (return x) (return y)) p q >>= id
