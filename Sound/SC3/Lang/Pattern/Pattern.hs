{-# LANGUAGE ExistentialQuantification #-}

module Sound.SC3.Lang.Pattern.Pattern
    ( P
    , evalP, pureP
    , pnil
    , pfix
    , pcontinue
    , pmap -- Prelude.fmap
    , preturn -- Control.Monad.return
    , pbind -- Control.Monad.(>>=)
    , pempty -- Data.Monoid.mempty
    , pappend -- Data.Monoid.mappend
    , ppure -- Control.Applicative.pure
    , papp -- Control.Applicative.(<*>)
    , prvalue
    , pappl
    , pacc
    , pinf
    , pzipWith
    , pcycle
    , prepeat ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import System.Random

-- | Empty indicates a (sub)pattern has no more elements, Nil causes
-- | the pattern to terminate.
data Reason = Empty | Nil

data P a = End Reason
         | Value a
         | RValue (StdGen -> (P a, StdGen))
         | Append (P a) (P a)
         | Fix StdGen (P a)
         | forall x . Continue (P x) (x -> P x -> P a)
         | forall x . App (P (x -> a)) (P x)
         | forall x y . Acc (x -> Maybe y -> (x, a)) x (P y)
         | forall x . AppL (P (x -> a)) (P x) (P (x -> a)) (P x) Bool Bool

data Result a = Result a (P a)
              | Done Reason

step :: StdGen -> P a -> (StdGen, Result a)
step g (End a) = (g, Done a)
step g (Value a) = (g, Result a pempty)
step g (RValue f) = let (p, g') = f g
                    in step g' p
step g (Append x y) = case step g x of
    (g', Done Nil) -> (g', Done Nil)
    (g', Done Empty) -> step g' y
    (g', Result a x') -> (g', Result a (Append x' y))
step g (Fix fg p) = case step fg p of
    (_, Done a) -> (g, Done a)
    (fg', Result x p') -> (g, Result x (Fix fg' p'))
step g (Continue p f) = case step g p of
    (g', Done a) -> (g', Done a)
    (g', Result x p') -> step g' (f x p')
step g (App p q) = case step g p of
    (g', Done a) -> (g', Done a)
    (g', Result f p') -> case step g' q of
        (g'', Done a) -> (g'', Done a)
        (g'', Result x q') -> (g'', Result (f x) (App p' q'))
step g (Acc f i p) = case step g p of
    (g', Done Nil) -> (g', Done Nil)
    (g', Done Empty) -> (g', Result q (End Empty)) where (_,q) = f i Nothing
    (g', Result a p') -> (g', Result q (Acc f j p')) where (j,q) = f i (Just a)
step g (AppL p q pr qr ph qh) = case step g p of
    (g', Done Nil) -> (g', Done Nil)
    (g', Done Empty) -> if qh 
                        then (g', Done Empty) 
                        else step g' (AppL pr q pr qr True qh)
    (g', Result f p') -> case step g' q of
        (g'', Done Nil) -> (g'', Done Nil)
        (g'', Done Empty) -> if ph 
                             then (g'', Done Empty) 
                             else step g'' (AppL p qr pr qr ph True)
        (g'', Result x q') -> (g'', Result (f x) (AppL p' q' pr qr ph qh))

pfoldr' :: StdGen -> (a -> b -> b) -> b -> P a -> b
pfoldr' g f i p = case step g p of
                    (_, Done _) -> i
                    (g', Result a p') -> f a (pfoldr' g' f i p')

pfoldr :: Seed -> (a -> b -> b) -> b -> P a -> b
pfoldr n = pfoldr' (mkStdGen n) 

evalP :: Int -> P a -> [a]
evalP n = pfoldr n (:) []

pureP :: P a -> [a]
pureP = evalP 0

instance (Show a) => Show (P a) where
    show = show . evalP 0

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
pcycle x = mappend x (pcycle x)

prepeat :: a -> P a
prepeat = pcycle . return

pmap :: (a -> b) -> P a -> P b
pmap f = (<*>) (prepeat f)

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
    (<*>) = papp

-- * Basic constructors

pnil :: P a
pnil = End Nil

pempty :: P a
pempty = End Empty

preturn :: a -> P a
preturn = Value

prvalue :: (StdGen -> (P a, StdGen)) -> P a
prvalue = RValue

pinf :: P Int
pinf = return 83886028 -- 2 ^^ 23

pappend :: P a -> P a -> P a
pappend = Append

type Seed = Int

pfix :: Seed -> P a -> P a
pfix n = Fix (mkStdGen n)

pcontinue :: P x -> (x -> P x -> P a) -> P a
pcontinue = Continue

pbind :: P x -> (x -> P a) -> P a
pbind p f = pcontinue p f'
    where f' x p' = mappend (f x) (pbind p' f)

papp :: P (a -> b) -> P a -> P b
papp = App

pacc :: (x -> Maybe y -> (x, a)) -> x -> P y -> P a
pacc = Acc

pappl :: P (a -> b) -> P a -> P b
pappl f p = AppL f p f p False False
