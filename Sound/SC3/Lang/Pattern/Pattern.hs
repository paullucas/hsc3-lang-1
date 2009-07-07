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

data P s a = Empty
           | Value a
           | RP (s -> (P s a, s))
           | Append (P s a) (P s a)
           | Fix s (P s a)
           | forall x . Unfoldr (x -> Maybe (a, x)) x
           | forall x . Continue (P s x) (x -> P s x -> P s a)
           | forall x . Apply (P s (x -> a)) (P s x)
           | forall x y . Scan (x -> y -> (x, a)) (Maybe (x -> a)) x (P s y)

data Result s a = Result s a (P s a)
                | Done s

step :: s -> P s a -> Result s a
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

pfoldr :: s -> (a -> b -> b) -> b -> P s a -> b
pfoldr g f i p = case step g p of
                   Done _ -> i
                   Result g' a p' -> f a (pfoldr g' f i p')

evalP :: P s a -> [a]
evalP = pfoldr (error "evalP") (:) []

instance (Show a) => Show (P s a) where
    show _ = show "a pattern"

instance (Eq a) => Eq (P s a) where
    _ == _ = False

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P s a -> P s b -> P s c
pzipWith f p = (<*>) (pure f <*> p)

instance (Num a) => Num (P s a) where
    (+) = pzipWith (+)
    (-) = pzipWith (-)
    (*) = pzipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional (P s a) where
    (/) = pzipWith (/)
    recip = fmap recip
    fromRational = return . fromRational

pcycle :: P s a -> P s a
pcycle x = x `mappend` pcycle x

prepeat :: a -> P s a
prepeat = pcycle . return

pmap :: (a -> b) -> P s a -> P s b
pmap = (<*>) . prepeat

instance Functor (P s) where
    fmap = pmap

instance Monad (P s) where
    (>>=) = pbind
    return = preturn

instance Monoid (P s a) where
    mempty = pempty
    mappend = pappend

ppure :: a -> P s a
ppure = prepeat

instance Applicative (P s) where
    pure = ppure
    (<*>) = papply

-- * Basic constructors

pempty :: P s a
pempty = Empty

preturn :: a -> P s a
preturn = Value

prp :: (s -> (P s a, s)) -> P s a
prp = RP

pinf :: P s Int
pinf = return 83886028 -- 2 ^^ 23

pappend :: P s a -> P s a -> P s a
pappend = Append

pfix :: s -> P s a -> P s a
pfix = Fix

pcontinue :: P s x -> (x -> P s x -> P s a) -> P s a
pcontinue = Continue

pbind :: P s x -> (x -> P s a) -> P s a
pbind p f = pcontinue p (\x q -> f x `mappend` pbind q f)

papply :: P s (a -> b) -> P s a -> P s b
papply = Apply

pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P s y -> P s a
pscan = Scan

punfoldr :: (x -> Maybe (a, x)) -> x -> P s a
punfoldr = Unfoldr
