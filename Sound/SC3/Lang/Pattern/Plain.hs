{-# Language GeneralizedNewtypeDeriving,FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.Plain where

import Control.Applicative
import Data.Foldable
import Data.List as L
import Data.Monoid
import Data.Traversable
import Sound.OpenSoundControl
import Sound.SC3.Server
import Sound.SC3.Lang.Collection.Event
import Sound.SC3.Lang.Collection.Numerical.Extending ()
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import Sound.SC3.Lang.Math.Datum ()
import qualified Sound.SC3.Lang.Math.SimpleNumber as M
import qualified Sound.SC3.Lang.Pattern.List as L
import System.Random

-- * P type and instances

newtype P a = P {unP :: [a]}
    deriving (Eq
             ,Functor
             ,Monoid
             ,Foldable
             ,Traversable
             ,Monad
             ,Num
             ,Fractional
             ,Floating
             ,Show)

instance Applicative P where
    pure = P . return
    P f <*> P e = P (map (\(f',e') -> f' e') (C.zip_c f e))

inf :: Int
inf = maxBound

-- * P lifting

liftP :: ([a] -> [b]) -> P a -> P b
liftP f = P . f . toList

liftP2 :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2 f a b = P (f (toList a) (toList b))

liftP3 :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3 f a b c = P (f (toList a) (toList b) (toList c))

-- * P functions

pnull :: P a -> Bool
pnull = null . toList

fromList :: [a] -> P a
fromList = P

prepeat :: a -> P a
prepeat = P . repeat

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f = liftP2 (C.zipWith_c f)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f = liftP3 (C.zipWith3_c f)

pzip :: P a -> P b -> P (a,b)
pzip = liftP2 C.zip_c

-- * SC3 patterns

pbind :: [(String,P Datum)] -> P (Event Datum)
pbind xs = P (L.pbind (map (\(k,v) -> (k,unP v)) xs))

pclutch :: P a -> P Bool -> P a
pclutch = liftP2 L.pclutch

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = liftP3 L.pdegreeToKey

pedit :: Key -> (a -> a) -> P (Event a) -> P (Event a)
pedit k f = fmap (e_edit k f)

pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (M.exprand l r) (pwhite e l r n)

pfinval :: Int -> P a -> P a
pfinval = ptake

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s = P . L.pgeom i s

pif :: P Bool -> P a -> P a -> P a
pif = liftP3 L.pif

place :: [P a] -> Int -> P a
place a n = P (L.place (map unP a) n)

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

prand :: Enum e => e -> [P a] -> Int -> P a
prand e a n = P (L.prand e (map unP a) n)

preject :: (a -> Bool) -> P a -> P a
preject f = liftP (L.preject f)

prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate = liftP2 L.prorate

pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

pseq :: [P a] -> Int -> P a
pseq a i = pconcat (L.concat (replicate i a))

pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s = P . L.pseries i s

pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a n = P (L.pshuf e a n)

pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide a n j s i = P . L.pslide a n j s i

pstutter :: P Int -> P a -> P a
pstutter = liftP2 L.pstutter

pswitch :: [P a] -> P Int -> P a
pswitch l = liftP (L.pswitch (map unP l))

pswitch1 :: [P a] -> P Int -> P a
pswitch1 l = liftP (L.pswitch1 (map unP l))

pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2 (L.pwhite' e)

pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = P . L.pwhite e l r

pwrand :: Enum e => e -> [P a] -> [Double] -> Int -> P a
pwrand e a w n = P (L.pwrand e (map unP a) w n)

pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap xs l r = P (L.pwrap (unP xs) l r)

pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = P (L.pxrand e (map unP a) n)

-- * Type specific aliases

pappend :: P a -> P a -> P a
pappend = mappend

pempty :: P a
pempty = mempty

-- * Haskell/SC3 aliases

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pfilter :: (a -> Bool) -> P a -> P a
pfilter = pselect

ptake :: Int -> P a -> P a
ptake n = liftP (take n)

-- * Non-SC3 patterns

pbool :: (Ord a,Num a) => P a -> P Bool
pbool = L.pbool

pconcat :: [P a] -> P a
pconcat = P . L.concat . map unP

pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = pconcat . replicate i

pcountpost :: P Bool -> P Int
pcountpost = liftP L.pcountpost

pcountpre :: P Bool -> P Int
pcountpre = liftP L.pcountpre

pcycle :: P a -> P a
pcycle = liftP cycle

pdrop :: Int -> P a -> P a
pdrop n = P . drop n . unP

pinterleave :: P a -> P a -> P a
pinterleave = liftP2 L.pinterleave

preplicate :: Int -> a -> P a
preplicate n = fromList . replicate n

prsd :: Eq a => P a -> P a
prsd = liftP L.prsd

ptail :: P a -> P a
ptail = pdrop 1

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger = liftP2 L.ptrigger

instance Audible (P (Event Datum)) where
    play fd = L.e_play fd . unP
