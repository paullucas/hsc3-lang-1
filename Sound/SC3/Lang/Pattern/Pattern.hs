{-# LANGUAGE ExistentialQuantification #-}

module Sound.SC3.Lang.Pattern.Pattern
    ( P
    , pfoldr, evalP
    , pfix
    , pcontinue
    , pmap -- Prelude.fmap
    , punfoldr -- Data.List.unfoldr
    , preturn -- Control.Monad.return
    , pbind -- Control.Monad.(>>=)
    , pempty -- Data.Monoid.mempty
    , pappend -- Data.Monoid.mappend
    , ppure -- Control.Applicative.pure
    , papply -- Control.Applicative.(<*>)
    , prp
    , pscan
    , pinf
    , pzipWith
    , pcycle
    , prepeat ) where

import Control.Applicative
import Data.Monoid
import qualified System.Random as R

data P a = Empty
         | Value a
         | RP (R.StdGen -> (P a, R.StdGen))
         | Append (P a) (P a)
         | Fix R.StdGen (P a)
         | forall x . Unfoldr (x -> Maybe (a, x)) x
         | forall x . Continue (P x) (x -> P x -> P a)
         | forall x . Apply (P (x -> a)) (P x)
         | forall x y . Scan (x -> y -> (x, a)) (Maybe (x -> a)) x (P y)

data Result a = Result R.StdGen a (P a)
              | Done R.StdGen

step :: R.StdGen -> P a -> Result a
step g Empty = Done g
step g (Value a) = Result g a pempty
step g (RP f) = let (p, g') = f g
                in step g' p
step g (Append x y) = case step g x of
    Done g' -> step g' y
    Result g' a x' -> Result g' a (Append x' y)
step g (Fix fg p) = case step fg p of
    Done _ -> Done g
    Result fg' x p' -> Result g x (Fix fg' p')
step g (Continue p f) = case step g p of
    Done g' -> Done g'
    Result g' x p' -> step g' (f x p')
step g (Unfoldr f x) = let y = f x 
                       in case y of
                            Nothing -> Done g
                            Just (a, x') -> Result g a (Unfoldr f x')
step g (Apply p q) = case step g p of
    Done g' -> Done g'
    Result g' f p' -> case step g' q of
        Done g'' -> Done g''
        Result g'' x q' -> Result g'' (f x) (Apply p' q')
step g (Scan f f' i p) = case step g p of
    Done g' -> case f' of
                 Just h -> Result g' (h i) Empty
                 Nothing -> Done g'
    Result g' a p' -> let (j, x) = f i a
                      in Result g' x (Scan f f' j p')

pfoldr' :: R.StdGen -> (a -> b -> b) -> b -> P a -> b
pfoldr' g f i p = case step g p of
                    Done _ -> i
                    Result g' a p' -> f a (pfoldr' g' f i p')

pfoldr :: Seed -> (a -> b -> b) -> b -> P a -> b
pfoldr = pfoldr' . R.mkStdGen

evalP :: Int -> P a -> [a]
evalP n = pfoldr n (:) []

instance (Show a) => Show (P a) where
    show _ = show "a pattern"

instance (Eq a) => Eq (P a) where
    _ == _ = False

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p = (<*>) (pure f <*> p)

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

pcycle :: P a -> P a
pcycle x = x `mappend` pcycle x

prepeat :: a -> P a
prepeat = pcycle . return

pmap :: (a -> b) -> P a -> P b
pmap = (<*>) . prepeat

instance Functor P where
    fmap = pmap

instance Monad P where
    (>>=) = pbind
    return = preturn

instance Monoid (P a) where
    mempty = pempty
    mappend = pappend

ppure :: a -> P a
ppure = prepeat

instance Applicative P where
    pure = ppure
    (<*>) = papply

-- * Basic constructors

pempty :: P a
pempty = Empty

preturn :: a -> P a
preturn = Value

prp :: (R.StdGen -> (P a, R.StdGen)) -> P a
prp = RP

pinf :: P Int
pinf = return 83886028 -- 2 ^^ 23

pappend :: P a -> P a -> P a
pappend = Append

type Seed = Int

pfix :: Seed -> P a -> P a
pfix = Fix . R.mkStdGen

pcontinue :: P x -> (x -> P x -> P a) -> P a
pcontinue = Continue

pbind :: P x -> (x -> P a) -> P a
pbind p f = pcontinue p (\x q -> f x `mappend` pbind q f)

papply :: P (a -> b) -> P a -> P b
papply = Apply

pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P y -> P a
pscan = Scan

punfoldr :: (x -> Maybe (a, x)) -> x -> P a
punfoldr = Unfoldr
