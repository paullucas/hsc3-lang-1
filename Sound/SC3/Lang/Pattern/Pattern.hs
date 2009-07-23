{-# LANGUAGE ExistentialQuantification #-}

module Sound.SC3.Lang.Pattern.Pattern
    ( P
    , evalP
    , pcontinue
    , punfoldr -- Data.List.unfoldr
    , prp
    , pscan
    , pinf
    , pzipWith
    , pcycle
    , prepeat ) where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import qualified Data.Traversable as T
import qualified System.Random as R

data P a = Empty
         | Value a
         | RP (R.StdGen -> (P a, R.StdGen))
         | Append (P a) (P a)
         | forall x . Unfoldr (x -> Maybe (a, x)) x
         | forall x . Continue (P x) (x -> P x -> P a)
         | forall x . Apply (P (x -> a)) (P x)
         | forall x y . Scan (x -> y -> (x, a)) (Maybe (x -> a)) x (P y)

data Result a = Result R.StdGen a (P a)
              | Done R.StdGen

step :: R.StdGen -> P a -> Result a
step g Empty = Done g
step g (Value a) = Result g a M.mempty
step g (RP f) = let (p, g') = f g
                in step g' p
step g (Append x y) = case step g x of
    Done g' -> step g' y
    Result g' a x' -> Result g' a (Append x' y)
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
pfoldr' g f i p =
    case step g p of
      Done _ -> i
      Result g' a p' -> f a (pfoldr' g' f i p')

evalP :: P a -> [a]
evalP = F.foldr (:) []

instance (Show a) => Show (P a) where
    show _ = show "a pattern"

instance (Eq a) => Eq (P a) where
    _ == _ = False

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p = (A.<*>) (A.pure f A.<*> p)

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
pcycle x = x `M.mappend` pcycle x

prepeat :: a -> P a
prepeat = pcycle . return

instance Functor P where
    fmap = (A.<*>) . prepeat

instance M.Monad P where
    (>>=) p f = pcontinue p (\x q -> f x `M.mappend` (>>=) q f)
    return = Value

instance M.MonadPlus P where
    mzero = Empty
    mplus = Append

instance M.Monoid (P a) where
    mempty = Empty
    mappend = Append

instance F.Foldable P where
    foldr = pfoldr' (R.mkStdGen 1497285)

instance T.Traversable P where
    traverse f =
        let cons_f x ys = (Append . Value) A.<$> f x A.<*> ys
        in F.foldr cons_f (A.pure Empty)

instance A.Applicative P where
    pure = prepeat
    (<*>) = Apply

instance A.Alternative P where
    empty = Empty
    (<|>) = Append

-- * Basic constructors

prp :: (R.StdGen -> (P a, R.StdGen)) -> P a
prp = RP

pinf :: P Int
pinf = return 83886028 -- 2 ^^ 23

pcontinue :: P x -> (x -> P x -> P a) -> P a
pcontinue = Continue

pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P y -> P a
pscan = Scan

punfoldr :: (x -> Maybe (a, x)) -> x -> P a
punfoldr = Unfoldr
