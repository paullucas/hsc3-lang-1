{-# Language FlexibleInstances #-}
-- | @sclang@ pattern library functions.
-- See <http://rd.slavepianos.org/?t=hsc3-texts> for tutorial.
module Sound.SC3.Lang.Pattern.ID where

import Control.Applicative hiding ((<*)) {- base -}
import Control.Monad {- base -}
import Data.Bifunctor {- bifunctors -}
import qualified Data.Foldable as F {- base -}
import qualified Data.List as L {- base -}
import qualified Data.List.Split as S {- split -}
import Data.Maybe {- base -}
import Data.Monoid {- base -}
import Sound.OSC {- hsc3 -}
import Sound.SC3 {- hsc3 -}
import System.Random {- random -}

import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Control.Event as E
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Math as M
import qualified Sound.SC3.Lang.Pattern.List as P
import qualified Sound.SC3.Lang.Random.Gen as R

-- * P

-- | Patterns are opaque.
--
-- Patterns are 'Functor's.
--
-- > fmap (* 2) [1,2,3,4,5] == [2,4,6,8,10]
-- > fmap (* 2) (toP [1,2,3,4,5]) == toP [2,4,6,8,10]
--
-- Patterns are 'Monoid's.
--
-- > mempty :: P ()
-- > mempty <> mempty == pempty
-- > mempty <> 1 == 1 <> pempty
-- > 1 <> 2 == toP [1,2]
-- > toP [1,2,3] <> toP [4,5,6] == toP [1,2,3,4,5,6]
-- > [1,2,3] <> [4,5,6] == [1,2,3,4,5,6]
--
-- > take 3 (concat (repeat [1,2])) == [1,2,1]
-- > ptake 3 (mconcat (repeat (toP [1,2]))) == toP [1,2,1]
-- > ptake 3 (mconcat [pseq [1,2] 1,pseq [3,4] 1]) == toP [1,2,3]
-- > ptake 3 (mconcat (repeat (toP [1,2]))) == toP [1,2,1]
--
-- Patterns are 'Applicative'.
--
-- > liftA2 (+) (toP [1,2]) (toP [3,4]) == toP [4,6]
-- > liftA2 (+) (toP [1,2,3]) (toP [4,5,6,7]) == toP [5,7,9]
--
-- The pattern instance is linear not combinatorial.  That is as for
-- 'ZipList', not as for list.
--
-- > getZipList (liftA2 (+) (ZipList [1,3,5]) (ZipList [6,4,2])) == [7,7,7]
-- > (pure (+) <*> [1,3,5] <*> [6,4,2]) == [7,5,3,9,7,5,11,9,7]
-- > (pure (+) <*> toP [1,3,5] <*> toP [6,4,2]) == toP [7,7,7]
-- > ppure 0 /= preturn 0
-- > pure 1 == [1] && return 1 == [1]
--
-- Patterns are 'Monad's.
--
-- > (pure 1 >>= return . id) == [1]
-- > (undecided 1 >>= undecided . id) == undecided 1
--
-- > take 3 (join (repeat [1,2])) == [1,2,1]
-- > ptake 3 (pjoin (prepeat (toP [1,2]))) == toP [1,2,1]
--
-- > do {x <- toP [1,2]
-- >    ;y <- toP [3,4,5]
-- >    ;return (x,y)} == toP [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
--
-- > liftM2 (+) (toP [0,1]) (toP [0,2]) == toP [0,2,1,3]
-- > mfilter even (toP [1,2,3,4]) == toP [2,4]
--
-- Patterns are 'MonadPlus'.
--
-- > msum [toP [1,2,3],toP [4,5,6]] == toP [1,2,3,4,5,6]
--
-- Patterns are 'Num'erical.
--
-- > 1 :: P Int
-- > (1 :: P Int) + 2 == 3
-- > 1 + toP [2,3,4] == toP [3,4,5]
-- > toP [1,2,3] + 2 == toP [3,4,5]
-- > toP [1,3,5] + toP [6,4,2] == toP [7,7,7]
-- > toP [1,2] + toP [3,4,5] == toP [4,6]
data P a = P {unP_either :: Either a [a]}
           deriving (Eq,Show)

-- | Lift a value to a pattern deferring deciding if the constructor
-- ought to be 'pure' or 'return' to the consuming function.
undecided :: a -> P a
undecided a = P (Left a)

-- | List to pattern, inverse is 'unP'.
--
-- > unP (toP "str") == "str"
toP :: [a] -> P a
toP = P . Right

-- | Pattern to list.  'undecided' values are singular.
--
-- > unP (undecided 'a') == ['a']
-- > unP (return 'a') == ['a']
-- > "aaa" `L.isPrefixOf` unP (pure 'a')
unP :: P a -> [a]
unP = either return id . unP_either

-- | Variant of 'unP' where 'undecided' values are 'repeat'ed.
--
-- > unP_repeat (return 'a') == ['a']
-- > take 2 (unP_repeat (undecided 'a')) == ['a','a']
-- > take 2 (unP_repeat (pure 'a')) == ['a','a']
unP_repeat :: P a -> [a]
unP_repeat = either repeat id . unP_either

instance Functor P where
    fmap f (P p) = P (bimap f (map f) p)

instance Monoid (P a) where
    mappend p q = toP (unP p ++ unP q)
    mempty = toP []

instance Applicative P where
    pure = toP . repeat
    f <*> e = pzipWith ($) f e

instance Alternative P where
    empty = mempty
    (<|>) = mappend

instance F.Foldable P where
    foldr f i p = L.foldr f i (unP p)

instance Monad P where
    m >>= k =
        case m of
          P (Left e) -> k e
          P (Right l) -> L.foldr (mappend . k) mempty l
    return x = toP [x]

instance MonadPlus P where
    mzero = mempty
    mplus = mappend

instance (Num a) => Num (P a) where
    (+) = pzipWith (+)
    (-) = pzipWith (-)
    (*) = pzipWith (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = undecided . fromInteger

instance (Fractional a) => Fractional (P a) where
    (/) = pzipWith (/)
    recip = fmap recip
    fromRational = undecided . fromRational

instance (Ord a) => Ord (P a) where
    (>) = error ("~> OrdE.>*")
    (>=) = error ("~> OrdE.>=*")
    (<) = error ("~> OrdE.<*")
    (<=) = error ("~> OrdE.<=*")

instance (OrdE a) => OrdE (P a) where
    (>*) = pzipWith (>*)
    (>=*) = pzipWith (>=*)
    (<*) = pzipWith (<*)
    (<=*) = pzipWith (<=*)

-- * Lift P

-- | Lift unary list function to pattern function.
liftP :: ([a] -> [b]) -> P a -> P b
liftP f = toP . f . unP

-- | Lift binary list function to pattern function.
liftP2 :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2 f p q =
    let p' = unP p
        q' = unP q
    in toP (f p' q')

-- | Lift binary list function to /implicitly repeating/ pattern function.
liftP2_repeat :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2_repeat f p q =
    let p' = unP_repeat p
        q' = unP_repeat q
    in toP (f p' q')

-- | Lift ternary list function to pattern function.
liftP3 :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3 f p q r =
    let p' = unP p
        q' = unP q
        r' = unP r
    in toP (f p' q' r')

-- | Lift ternary list function to /implicitly repeating/ pattern function.
liftP3_repeat :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3_repeat f p q r =
    let p' = unP_repeat p
        q' = unP_repeat q
        r' = unP_repeat r
    in toP (f p' q' r')

-- * Zip P

-- | An /implicitly repeating/ pattern variant of 'zipWith'.
--
-- > zipWith (*) [1,2,3] [5,6] == [5,12]
-- > pzipWith (*) (toP [1,2,3]) (toP [5,6]) == toP [5,12]
--
-- It is the basis for lifting binary operators to patterns.
--
-- > toP [1,2,3] * toP [5,6] == toP [5,12]
--
-- > let p = pzipWith (,) (pseq [1,2] 2) (pseq [3,4] inf)
-- > in p == toP [(1,3),(2,4),(1,3),(2,4)]
--
-- > zipWith (,) (return 0) (return 1) == return (0,1)
-- > pzipWith (,) 0 1 == undecided (0,1)
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p q =
    case (p,q) of
      (P (Left m),P (Left n)) -> undecided (f m n)
      _ -> toP (zipWith f (unP_repeat p) (unP_repeat q))

-- | An /implicitly repeating/ pattern variant of 'zipWith3'.
pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f p q r =
    case (p,q,r) of
      (P (Left m),P (Left n),P (Left o)) -> undecided (f m n o)
      _ -> toP (zipWith3 f (unP_repeat p) (unP_repeat q) (unP_repeat r))

-- | An /implicitly repeating/ pattern variant of 'zip'.
--
-- > zip (return 0) (return 1) == return (0,1)
-- > pzip (undecided 3) (undecided 4) == undecided (3,4)
-- > pzip 0 1 == undecided (0,1)
--
-- Note that 'pzip' is otherwise like haskell 'zip', ie. truncating.
--
-- > zip [1,2] [0] == [(1,0)]
-- > pzip (toP [1,2]) (return 0) == toP [(1,0)]
-- > pzip (toP [1,2]) (pure 0) == toP [(1,0),(2,0)]
-- > pzip (toP [1,2]) 0 == toP [(1,0),(2,0)]
pzip :: P a -> P b -> P (a,b)
pzip = pzipWith (,)

-- | Pattern variant of 'zip3'.
pzip3 :: P a -> P b -> P c -> P (a,b,c)
pzip3 = pzipWith3 (,,)

-- | Pattern variant on 'unzip'.
--
-- > let p = punzip (pzip (toP [1,2,3]) (toP [4,5]))
-- > in p == (toP [1,2],toP [4,5])
punzip :: P (a,b) -> (P a,P b)
punzip p = let (i,j) = unzip (unP p) in (toP i,toP j)

-- * Math

-- | Type specialised 'maxBound', a pseudo-/infinite/ value for use at
-- pattern repeat counts.
--
-- > inf == maxBound
inf :: Int
inf = maxBound

-- | Constant /NaN/ (not a number) value for use as a rest indicator
-- at a frequency model input (not at a @rest@ key).
--
-- > isNaN nan == True
nan :: Floating a => a
nan = sqrt (-1)

-- * Data.List patterns

-- | Pattern variant of ':'.
--
-- > pcons 'α' (pn (return 'β') 2) == toP "αββ"
pcons :: a -> P a -> P a
pcons = mappend . return

-- | Pattern variant of 'L.null'.
--
-- > pnull mempty == True
-- > pnull (undecided 'a') == False
-- > pnull (pure 'a') == False
-- > pnull (return 'a') == False
pnull :: P a -> Bool
pnull = null . unP

-- | Alias for 'pure'.
--
-- > ptake 5 (prepeat 3) == toP [3,3,3,3,3]
-- > ptake 5 (pure 3) == toP [3,3,3,3,3]
-- > take 5 (pure 3) == [3]
prepeat :: a -> P a
prepeat = pure

-- | Pattern variant of 'P.take_inf', see also 'pfinval'.
--
-- > ptake 5 (pseq [1,2,3] 2) == toP [1,2,3,1,2]
-- > ptake 5 (toP [1,2,3]) == toP [1,2,3]
-- > ptake 5 (pseq [1,2,3] inf) == toP [1,2,3,1,2]
-- > ptake 5 (pwhite 'α' 0 5 inf) == toP [5,2,1,2,0]
--
-- Note that `ptake` does not extend the input pattern, unlike `pser`.
--
-- > ptake 5 (toP [1,2,3]) == toP [1,2,3]
-- > pser [1,2,3] 5 == toP [1,2,3,1,2]
ptake :: Int -> P a -> P a
ptake n = liftP (P.take_inf n)

-- | Type specialised 'P.mcycle'.
--
-- > ptake 5 (pcycle 1) == preplicate 5 1
-- > ptake 5 (pcycle (pure 1)) == preplicate 5 1
-- > ptake 5 (pcycle (return 1)) == preplicate 5 1
pcycle :: P a -> P a
pcycle = P.mcycle

-- | Lifted 'L.drop'.
--
-- > > p = Pseries(1,1,20).drop(5);
-- > > p.asStream.all == [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--
-- > pdrop 5 (pseries 1 1 10) == toP [6,7,8,9,10]
-- > pdrop 1 mempty == mempty
pdrop :: Int -> P a -> P a
pdrop n = liftP (drop n)

-- | Pattern variant of `filter`.  Allows values for which the
-- predicate is true.  Aliased to `pselect`.  See also `preject`.
--
-- > pfilter (< 3) (pseq [1,2,3] 2) == toP [1,2,1,2]
pfilter :: (a -> Bool) -> P a -> P a
pfilter f = liftP (filter f)

-- | Pattern variant of `replicate`.
--
-- > preplicate 4 1 == toP [1,1,1,1]
--
-- Compare to `pn`:
--
-- > pn 1 4 == toP [1,1,1,1]
-- > pn (toP [1,2]) 3 == toP [1,2,1,2,1,2]
-- > preplicate 4 (toP [1,2]) :: P (P Int)
preplicate :: Int -> a -> P a
preplicate n = toP . (if n == inf then repeat else replicate n)

-- | Pattern variant of `scanl`.  `scanl` is similar to `foldl`, but
-- returns a list of successive reduced values from the left.
--
-- > Data.Foldable.foldl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == (43)
-- > pscanl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == toP [4,9,20,43]
pscanl :: (a -> b -> a) -> a -> P b -> P a
pscanl f i = liftP (L.scanl f i)

-- | 'pdrop' @1@.  Note that 'tail' is partial
--
-- > ptail (toP [1,2]) == toP [2]
-- > ptail mempty == mempty
ptail :: P a -> P a
ptail = pdrop 1

-- | Variant of 'L.transpose'.
--
-- > L.transpose [[1,2],[3,4,5]] == [[1,3],[2,4],[5]]
-- > ptranspose [toP [1,2],toP [3,4,5]] == toP [[1,3],[2,4],[5]]
--
-- > let p = ptranspose [pseq [1,2] inf,pseq [4,5] inf]
-- > in ptake 2 (pdrop (2^16) p) == toP [[1,4],[2,5]]
ptranspose :: [P a] -> P [a]
ptranspose l = toP (L.transpose (map unP l))

-- | An /implicitly repeating/ pattern variant of 'P.transpose_st'.
ptranspose_st_repeat :: [P a] -> P [a]
ptranspose_st_repeat l = toP (P.transpose_st (map unP_repeat l))

-- * SC3 Collection patterns

-- | Variant of 'C.flop'.
--
-- > pflop' [toP [1,2],toP [3,4,5]] == toP [[1,3],[2,4],[1,5]]
-- > pflop' [toP [1,2],3] == toP [[1,3],[2,3]]
-- > pflop' [pseq [1,2] 1,pseq [3,4] inf]
pflop' :: [P a] -> P [a]
pflop' l = toP (C.flop (map unP l))

-- | 'fmap' 'toP' of 'pflop''.
--
-- > C.flop [[1,2],[3,4,5]] == [[1,3],[2,4],[1,5]]
-- > pflop [toP [1,2],toP [3,4,5]] == toP (map toP [[1,3],[2,4],[1,5]])
pflop :: [P a] -> P (P a)
pflop = fmap toP . pflop'

-- * SC3 patterns

-- | Lifted /implicitly repeating/ 'P.pbrown''.
--
-- > pbrown' 'α' 1 700 (pseq [1,20] inf) 4 == toP [415,419,420,428]
pbrown' :: (Enum e,Random n,Num n,Ord n) =>
           e -> P n -> P n -> P n -> Int -> P n
pbrown' e l r s n =
    let f = liftP3_repeat (P.brown' e)
    in ptake n (f l r s)

-- | Lifted 'P.brown'.
--
-- > pbrown 'α' 0 9 1 5 == toP [4,4,5,4,3]
pbrown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> Int -> P n
pbrown e l r s n = ptake n (toP (P.brown e l r s))

-- | SC3 sample and hold pattern.  For true values in the control
-- pattern, step the value pattern, else hold the previous value.
--
-- > > c = Pseq([1,0,1,0,0,1,1],inf);
-- > > p = Pclutch(Pser([1,2,3,4,5],8),c);
-- > > r = [1,1,2,2,2,3,4,5,5,1,1,1,2,3];
-- > > p.asStream.all == r
--
-- > let {c = pbool (pseq [1,0,1,0,0,1,1] inf)
-- >     ;p = pclutch (pser [1,2,3,4,5] 8) c
-- >     ;r = toP [1,1,2,2,2,3,4,5,5,1,1,1,2,3]}
-- > in p == toP [1,1,2,2,2,3,4,5,5,1,1,1,2,3]
--
-- Note the initialization behavior, nothing is generated until the
-- first true value.
--
-- > let {p = pseq [1,2,3,4,5] 1
-- >     ;q = pbool (pseq [0,0,0,0,0,0,1,0,0,1,0,1] 1)}
-- > in pclutch p q == toP [1,1,1,2,2,3]
pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

-- | SC3 name for 'fmap', ie. patterns are functors.
--
-- > > Pcollect({|i| i * 3},Pseq(#[1,2,3],1)).asStream.all == [3,6,9]
-- > pcollect (* 3) (toP [1,2,3]) == toP [3,6,9]
--
-- > > Pseq(#[1,2,3],1).collect({|i| i * 3}).asStream.all == [3,6,9]
-- > fmap (* 3) (toP [1,2,3]) == toP [3,6,9]
pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

-- | SC3 pattern to constrain the sum of a numerical pattern.  Is
-- equal to /p/ until the accumulated sum is within /t/ of /n/.  At
-- that point, the difference between the specified sum and the
-- accumulated sum concludes the pattern.
--
-- > > p = Pconst(10,Pseed(Pn(1000,1),Prand([1,2,0.5,0.1],inf),0.001));
-- > > p.asStream.all == [0.5,0.1,0.5,1,2,2,0.5,1,0.5,1,0.9]
--
-- > let p = pconst 10 (prand 'α' [1,2,0.5,0.1] inf) 0.001
-- > in (p,Data.Foldable.sum p)
pconst :: (Ord a,Num a) => a -> P a -> a -> P a
pconst n p t =
    let f _ [] = []
        f j (i:is) = if i + j < n - t
                     then i : f (j + i) is
                     else [n - j]
    in toP (f 0 (unP p))

-- | SC3 pattern to derive notes from an index into a scale.
--
-- > let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
-- >     ;q = return [0,2,4,5,7,9,11]
-- >     ;r = [0,2,4,5,7,5,4,2,0,4,7,12,7,4,0,2,4,5,7,5,4,2,0,4,7,12,7,4]}
-- > in pdegreeToKey p q (return 12) == toP r
--
-- > let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
-- >     ;q = pseq (map return [[0,2,4,5,7,9,11],[0,2,3,5,7,8,11]]) 1
-- >     ;r = [0,2,4,5,7,5,4,2,0,4,7,12,7,4,0,2,3,5,7,5,3,2,0,3,7,12,7,3]}
-- > in pdegreeToKey p (pstutter 14 q) (return 12) == toP r
--
-- This is the pattern variant of 'M.degreeToKey'.
--
-- > let s = [0,2,4,5,7,9,11]
-- > in map (M.degreeToKey s 12) [0,2,4,7,4,2,0] == [0,4,7,12,7,4,0]
pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 (\i j k -> M.degreeToKey j k i)

-- | SC3 pattern to calculate adjacent element difference.
--
-- > > Pdiff(Pseq([0,2,3,5,6,8,9],1)).asStream.all == [2,1,2,1,2,1]
-- > pdiff (pseq [0,2,3,5,6,8,9] 1) == toP [2,1,2,1,2,1]
pdiff :: Num n => P n -> P n
pdiff p = ptail p - p

-- | Lifted 'P.durStutter'.
--
-- > > s = Pseq(#[1,1,1,1,1,2,2,2,2,2,0,1,3,4,0],inf);
-- > > d = Pseq(#[0.5,1,2,0.25,0.25],1);
-- > > PdurStutter(s,d).asStream.all == [0.5,1,2,0.25,0.25]
--
-- > let {s = pseq [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0] inf
-- >     ;d = pseq [0.5,1,2,0.25,0.25] 1}
-- > in pdurStutter s d == toP [0.5,1.0,2.0,0.25,0.25]
pdurStutter :: Fractional a => P Int -> P a -> P a
pdurStutter = liftP2 P.durStutter

-- | Lifted 'P.exprand'.
--
-- > > Pexprand(0.0001,1,10).asStream.all
-- > pexprand 'α' 0.0001 1 10
pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r = toP . P.exprand e l r

-- | Alias for 'ptake'
--
-- > > Pfinval(5,Pseq(#[1,2,3],inf)).asStream.all == [1,2,3,1,2]
-- > pfinval 5 (pseq [1,2,3] inf) == toP [1,2,3,1,2]
pfinval :: Int -> P a -> P a
pfinval = ptake

-- | Type specialised 'P.ffold'.
--
-- > pfold (toP [10,11,12,-6,-7,-8]) (-7) 11 == toP [10,11,10,-6,-7,-6]
pfold :: (RealFrac n) => P n -> n -> n -> P n
pfold = P.ffold

-- | A variant of the SC3 pattern that evaluates a closure at each
-- step.  The haskell variant function has a 'StdGen' form.
pfuncn :: Enum e => e -> (StdGen -> (n,StdGen)) -> Int -> P n
pfuncn e f n = toP (P.funcn e f n)

-- | SC3 geometric series pattern.
--
-- > > Pgeom(3,6,5).asStream.all == [3,18,108,648,3888]
-- > pgeom 3 6 5 == toP [3,18,108,648,3888]
--
-- > > Pgeom(1,2,10).asStream.all == [1,2,4,8,16,32,64,128,256,512]
-- > pgeom 1 2 10 == toP [1,2,4,8,16,32,64,128,256,512]
--
-- Real numbers work as well.
--
-- > > p = Pgeom(1.0,1.1,6).collect({|i| (i * 100).floor});
-- > > p.asStream.all == [100,110,121,133,146,161];
--
-- > let p = fmap (floor . (* 100)) (pgeom 1.0 1.1 6)
-- > in p == toP [100,110,121,133,146,161]
pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = toP (C.geom n i s)

-- | SC3 /implicitly repeating/ pattern-based conditional expression.
--
-- > > a = Pfunc({0.3.coin});
-- > > b = Pwhite(0,9,3);
-- > > c = Pwhite(10,19,3);
-- > > Pfin(9,Pif(a,b,c)).asStream.all
--
-- > let {a = fmap (< 0.75) (pwhite 'α' 0.0 1.0 inf)
-- >     ;b = pwhite 'β' 0 9 6
-- >     ;c = pwhite 'γ' 10 19 6}
-- > in pif a b c * (-1) == toP [-7,-3,-11,-17,-18,-6,-3,-4,-5]
pif :: P Bool -> P a -> P a -> P a
pif = liftP3_repeat P.if_demand

-- | SC3 interlaced embedding of subarrays.
--
-- > > Place([0,[1,2],[3,4,5]],3).asStream.all == [0,1,3,0,2,4,0,1,5]
-- > C.lace 9 [[0],[1,2],[3,4,5]] == [0,1,3,0,2,4,0,1,5]
-- > place [[0],[1,2],[3,4,5]] 3 == toP [0,1,3,0,2,4,0,1,5]
--
-- > > Place(#[1,[2,5],[3,6]],2).asStream.all == [1,2,3,1,5,6]
-- > C.lace 6 [[1],[2,5],[3,6]] == [1,2,3,1,5,6]
-- > place [[1],[2,5],[3,6]] 2 == toP [1,2,3,1,5,6]
--
-- > C.lace 12 [[1],[2,5],[3,6..]] == [1,2,3,1,5,6,1,2,9,1,5,12]
-- > place [[1],[2,5],[3,6..]] 4 == toP [1,2,3,1,5,6,1,2,9,1,5,12]
place :: [[a]] -> Int -> P a
place a n =
    let f = toP . concat . P.take_inf n . L.transpose . map cycle
    in f a

-- | SC3 pattern to lace input patterns.
--
-- > > p = Ppatlace([1,Pseq([2,3],2),4],5);
-- > > p.asStream.all == [1,2,4,1,3,4,1,2,4,1,3,4,1,4]
--
-- > ppatlace [1,pseq [2,3] 2,4] 5 == toP [1,2,4,1,3,4,1,2,4,1,3,4,1,4]
--
-- > > p = Ppatlace([1,Pseed(Pn(1000,1),Prand([2,3],inf))],5);
-- > > p.asStream.all == [1,3,1,3,1,3,1,2,1,2]
--
-- > ppatlace [1,prand 'α' [2,3] inf] 5 == toP [1,3,1,2,1,3,1,2,1,2]
ppatlace :: [P a] -> Int -> P a
ppatlace a n =
    let a' = L.transpose (map unP_repeat a)
    in toP (L.concat (P.take_inf n a'))

-- | SC3 pattern to repeat the enclosed pattern a number of times.
--
-- > pn 1 4 == toP [1,1,1,1]
-- > pn (toP [1,2,3]) 3 == toP [1,2,3,1,2,3,1,2,3]
--
-- This is related to `concat`.`replicate` in standard list processing.
--
-- > concat (replicate 4 [1]) == [1,1,1,1]
-- > concat (replicate 3 [1,2,3]) == [1,2,3,1,2,3,1,2,3]
--
-- There is a `pconcatReplicate` near-alias (reversed argument order).
--
-- > pconcatReplicate 4 1 == toP [1,1,1,1]
-- > pconcatReplicate 3 (toP [1,2]) == toP [1,2,1,2,1,2]
--
-- This is productive over infinite lists.
--
-- > concat (replicate inf [1])
-- > pconcat (replicate inf 1)
-- > pconcatReplicate inf 1
pn :: P a -> Int -> P a
pn p n = mconcat (replicate n p)

-- | Pattern variant of 'C.normalizeSum'.
pnormalizeSum :: Fractional n => P n -> P n
pnormalizeSum = liftP C.normalizeSum

-- | Un-joined variant of 'prand'.
--
-- > let p = prand' 'α' [1,toP [2,3],toP [4,5,6]] 5
-- > in p == toP [toP [4,5,6],toP [4,5,6],toP [2,3],toP [4,5,6],1]
prand' :: Enum e => e -> [P a] -> Int -> P (P a)
prand' e a n = toP (P.rand' e a n)

-- | SC3 pattern to make n random selections from a list of patterns,
-- the resulting pattern is flattened (joined).
--
-- > > p = Pseed(Pn(1000,1),Prand([1,Pseq([10,20,30]),2,3,4,5],6));
-- > > p.asStream.all == [3,5,3,10,20,30,2,2]
--
-- > prand 'α' [1,toP [10,20],2,3,4,5] 5 == toP [5,2,10,20,2,1]
prand :: Enum e => e -> [P a] -> Int -> P a
prand e a = join . prand' e a

-- | SC3 pattern to rejects values for which the predicate is true.  reject
-- f is equal to filter (not . f).
--
-- > preject (== 1) (pseq [1,2,3] 2) == toP [2,3,2,3]
-- > pfilter (not . (== 1)) (pseq [1,2,3] 2) == toP [2,3,2,3]
--
-- > > p = Pseed(Pn(1000,1),Pwhite(0,255,20).reject({|x| x.odd}));
-- > > p.asStream.all == [224,60,88,94,42,32,110,24,122,172]
--
-- > preject odd (pwhite 'α' 0 255 10) == toP [32,158,62,216,240,20]
--
-- > > p = Pseed(Pn(1000,1),Pwhite(0,255,20).select({|x| x.odd}));
-- > > p.asStream.all == [151,157,187,129,45,245,101,79,77,243]
--
-- > pselect odd (pwhite 'α' 0 255 10) == toP [241,187,119,127]
preject :: (a -> Bool) -> P a -> P a
preject f = liftP (filter (not . f))

-- | Underlying pattern for 'prorate'.
--
-- > prorate' (Left 0.6) 0.5
prorate' :: Num a => Either a [a] -> a -> P a
prorate' p =
    case p of
      Left p' -> toP . P.rorate_n' p'
      Right p' -> toP . P.rorate_l' p'

-- | SC3 sub-dividing pattern.
--
-- > > p = Prorate(Pseq([0.35,0.5,0.8]),1);
-- > > p.asStream.all == [0.35,0.65,0.5,0.5,0.8,0.2];
--
-- > let p = prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1
-- > in fmap roundE (p * 100) == toP [35,65,50,50,80,20]
--
-- > > p = Prorate(Pseq([0.35,0.5,0.8]),Pseed(Pn(100,1),Prand([20,1],inf)));
-- > > p.asStream.all == [7,13,0.5,0.5,16,4]
--
-- > let p = prorate (fmap Left (pseq [0.35,0.5,0.8] 1))
-- >                 (prand 'α' [20,1] 3)
-- > in fmap roundE (p * 100) == toP [35,65,1000,1000,80,20]
--
-- > > l = [[1,2],[5,7],[4,8,9]].collect(_.normalizeSum);
-- > > Prorate(Pseq(l,1)).asStream.all
--
-- > let l = map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]]
-- > in prorate (toP l) 1
--
-- > > Pfinval(5,Prorate(0.6,0.5)).asStream.all == [0.3,0.2,0.3,0.2,0.3]
--
-- > pfinval 5 (prorate (fmap Left 0.6) 0.5) == toP [0.3,0.2,0.3,0.2,0.3]
prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate p = pjoin_repeat . pzipWith prorate' p

-- | See 'pfilter'.
--
-- > pselect (< 3) (pseq [1,2,3] 2) == toP [1,2,1,2]
pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

-- | Variant of `pseq` that retrieves only one value from each pattern
-- on each list traversal.  Compare to `pswitch1`.
--
-- > pseq [pseq [1,2] 1,pseq [3,4] 1] 2 == toP [1,2,3,4,1,2,3,4]
-- > pseq1 [pseq [1,2] 1,pseq [3,4] 1] 2 == toP [1,3,2,4]
-- > pseq1 [pseq [1,2] inf,pseq [3,4] inf] 3 == toP [1,3,2,4,1,3]
pseq1 :: [P a] -> Int -> P a
pseq1 a i = join (ptake i (pflop a))

-- | SC3 pattern to cycle over a list of patterns. The repeats pattern
-- gives the number of times to repeat the entire list.
--
-- > pseq [return 1,return 2,return 3] 2 == toP [1,2,3,1,2,3]
-- > pseq [1,2,3] 2 == toP [1,2,3,1,2,3]
-- > pseq [1,pn 2 2,3] 2 == toP [1,2,2,3,1,2,2,3]
--
-- There is an 'inf' value for the repeats variable.
--
-- > ptake 3 (pdrop 1000000 (pseq [1,2,3] inf)) == toP [2,3,1]
pseq :: [P a] -> Int -> P a
pseq a i =
    let a' = mconcat a
    in if i == inf then pcycle a' else pn a' i

-- | A variant of 'pseq' that passes a new seed at each invocation,
-- see also 'pfuncn'.
--
-- > pseqr (\e -> [pshuf e [1,2,3,4] 1]) 2 == toP [2,3,4,1,4,1,2,3]
pseqr :: (Int -> [P a]) -> Int -> P a
pseqr f n = mconcat (L.concatMap f [1 .. n])

-- | A variant of 'pseq' to aid translating a common SC3 idiom where a
-- finite random pattern is included in a @Pseq@ list.  In the SC3
-- case, at each iteration a new computation is run.  This idiom does
-- not directly translate to the declarative haskell pattern library.
--
-- > > Pseq([1,Prand([2,3],1)],5).asStream.all
-- > pseq [1,prand 'α' [2,3] 1] 5 == toP [1,3,1,3,1,3,1,3,1,3]
--
-- Although the intended pattern can usually be expressed using an
-- alternate construction:
--
-- > > Pseq([1,Prand([2,3],1)],5).asStream.all
-- > ppatlace [1,prand 'α' [2,3] inf] 5 == toP [1,3,1,2,1,3,1,2,1,2]
--
-- the 'pseqn' variant handles many common cases.
--
-- > > Pseq([Pn(8,2),Pwhite(9,16,1)],5).asStream.all
--
-- > let p = pseqn [2,1] [8,pwhite 'α' 9 16 inf] 5
-- > in p == toP [8,8,10,8,8,9,8,8,12,8,8,15,8,8,15]
pseqn :: [Int] -> [P a] -> Int -> P a
pseqn n q =
    let rec p c = if c == 0
                  then mempty
                  else let (i,j) = unzip (zipWith psplitAt n p)
                       in mconcat i <> rec j (c - 1)
    in rec (map pcycle q)

-- | Variant of 'pser' that consumes sub-patterns one element per
-- iteration.
--
-- > pser1 [1,pser [10,20] 3,3] 9 == toP [1,10,3,1,20,3,1,10,3]
pser1 :: [P a] -> Int -> P a
pser1 a i = ptake i (join (pflop a))

-- | SC3 pattern that is like 'pseq', however the repeats variable
-- gives the number of elements in the sequence, not the number of
-- cycles of the pattern.
--
-- > pser [1,2,3] 5 == toP [1,2,3,1,2]
-- > pser [1,pser [10,20] 3,3] 9 == toP [1,10,20,10,3,1,10,20,10]
-- > pser [1,2,3] 5 * 3 == toP [3,6,9,3,6]
pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (mconcat a))

-- | SC3 arithmetric series pattern, see also 'pgeom'.
--
-- > pseries 0 2 10 == toP [0,2,4,6,8,10,12,14,16,18]
-- > pseries 9 (-1) 10 == toP [9,8 .. 0]
-- > pseries 1.0 0.2 3 == toP [1.0::Double,1.2,1.4]
pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = toP (C.series n i s)

-- | SC3 pattern to return @n@ repetitions of a shuffled sequence.
--
-- > > Pshuf([1,2,3,4],2).asStream.all
-- > pshuf 'α' [1,2,3,4] 2 == toP [2,4,3,1,2,4,3,1]
pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a =
    let (a',_) = R.scramble a (mkStdGen (fromEnum e))
    in pn (toP a')

-- | Lifted 'P.slide'.
--
-- > > Pslide([1,2,3,4],inf,3,1,0).asStream.all
-- > pslide [1,2,3,4] 4 3 1 0 True == toP [1,2,3,2,3,4,3,4,1,4,1,2]
-- > pslide [1,2,3,4,5] 3 3 (-1) 0 True == toP [1,2,3,5,1,2,4,5,1]
pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide a n j s i = toP . P.slide a n j s i

-- | Pattern variant of 'splitAt'.
psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p =
    let (i,j) = splitAt n (unP p)
    in (toP i,toP j)

-- | Pattern variant of 'S.splitPlaces'.
--
-- > psplitPlaces' (toP [1,2,3]) (pseries 1 1 6) == toP [[1],[2,3],[4,5,6]]
-- > psplitPlaces' (toP [1,2,3]) (toP ['a'..]) == toP ["a","bc","def"]
psplitPlaces' :: P Int -> P a -> P [a]
psplitPlaces' = liftP2 S.splitPlaces

-- | A variant of 'psplitPlaces'' that joins the output pattern.
--
-- > psplitPlaces (toP [1,2,3]) (toP ['a'..]) == toP (map toP ["a","bc","def"])
psplitPlaces :: P Int -> P a -> P (P a)
psplitPlaces n = fmap toP . psplitPlaces' n

-- | SC3 /implicitly repeating/ pattern to repeat each element of a
-- pattern /n/ times.
--
-- > > Pstutter(2,Pseq([1,2,3],1)).asStream.all == [1,1,2,2,3,3]
-- > pstutter 2 (pseq [1,2,3] 1) == toP [1,1,2,2,3,3]
--
-- The count input may be a pattern.
--
-- > let {p = pseq [1,2] inf
-- >     ;q = pseq [1,2,3] 2}
-- > in pstutter p q == toP [1,2,2,3,1,1,2,3,3]
--
-- > pstutter (toP [1,2,3]) (toP [4,5,6]) == toP [4,5,5,6,6,6]
-- > pstutter 2 (toP [4,5,6]) == toP [4,4,5,5,6,6]
pstutter :: P Int -> P a -> P a
pstutter = liftP2_repeat P.stutter

-- | Lifted 'P.switch'.
--
-- > let p = pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (toP [2,2,0,1])
-- > in p == toP [800,800,1,2,3,1,2,3,65,76]
pswitch :: [P a] -> P Int -> P a
pswitch l = liftP (P.switch (map unP l))

-- | Lifted /implicitly repeating/ 'P.switch1'.
--
-- > > l = [Pseq([1,2,3],inf),Pseq([65,76],inf),8];
-- > > p = Pswitch1(l,Pseq([2,2,0,1],3));
-- > > p.asStream.all == [8,8,1,65,8,8,2,76,8,8,3,65];
--
-- > let p = pswitch1 [pseq [1,2,3] inf
-- >                  ,pseq [65,76] inf
-- >                  ,8] (pseq [2,2,0,1] 6)
-- > in p == toP [8,8,1,65,8,8,2,76,8,8,3,65,8,8,1,76,8,8,2,65,8,8,3,76]
pswitch1 :: [P a] -> P Int -> P a
pswitch1 l = liftP (P.switch1 (map unP_repeat l))

-- | 'pseq' of 'ptranspose_st_repeat'.
--
-- > > l = [Pseries(7,-1,8),3,Pseq([9,7,4,2],1),Pseq([4,2,0,0,-3],1)];
-- > > p = Ptuple(l,1);
-- > > p.asStream.all == [[7,3,9,4],[6,3,7,2],[5,3,4,0],[4,3,2,0]]
--
-- > let p = ptuple [pseries 7 (-1) 8
-- >                ,3
-- >                ,pseq [9,7,4,2] 1
-- >                ,pseq [4,2,0,0,-3] 1] 1
-- > in p == toP [[7,3,9,4],[6,3,7,2],[5,3,4,0],[4,3,2,0]]
ptuple :: [P a] -> Int -> P [a]
ptuple p = pseq [ptranspose_st_repeat p]

-- | Lifted /implicitly repeating/ 'P.pwhite'.
--
-- > pwhite' 'α' 0 (pseq [9,19] 3) == toP [3,0,1,6,6,15]
pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2_repeat (P.white' e)

-- | Lifted 'P.white'.
--
-- > pwhite 'α' 0 9 5 == toP [3,0,1,6,6]
-- > pwhite 'α' 0 9 5 - pwhite 'α' 0 9 5 == toP [0,0,0,0,0]
pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = toP . P.white e l r

-- | Lifted 'P.whitei'.
--
-- > pwhitei 'α' 1 9 5 == toP [5,1,7,7,8]
pwhitei :: (RealFracE n,Random n,Enum e) => e -> n -> n -> Int -> P n
pwhitei e l r =  toP . P.whitei e l r

-- | Lifted 'P.wrand'.
--
-- > let w = C.normalizeSum [12,6,3]
-- > in pwrand 'α' [1,2,3] w 6 == toP [2,1,2,3,3,2]
--
-- > > r = Pwrand.new([1,2,Pseq([3,4],1)],[1,3,5].normalizeSum,6);
-- > > p = Pseed(Pn(100,1),r);
-- > > p.asStream.all == [2,3,4,1,3,4,3,4,2]
--
-- > let w = C.normalizeSum [1,3,5]
-- > in pwrand 'ζ' [1,2,pseq [3,4] 1] w 6 == toP [3,4,2,2,3,4,1,3,4]
pwrand :: (Enum e) => e -> [P a] -> [Double] -> Int -> P a
pwrand e a w = toP . P.wrand e (map unP a) w

-- | Type specialised 'P.fwrap', see also 'pfold'.
--
-- > > p = Pwrap(Pgeom(200,1.25,9),200,1000.0);
-- > > r = p.asStream.all.collect({|n| n.round});
-- > > r == [200,250,313,391,488,610,763,954,392];
--
-- > let p = fmap roundE (pwrap (pgeom 200 1.25 9) 200 1000)
-- > in p == toP [200,250,312,391,488,610,763,954,391]
pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap = P.fwrap

-- | Lifted 'P.xrand'.
--
-- > let p = pxrand 'α' [1,toP [2,3],toP [4,5,6]] 9
-- > in p == toP [4,5,6,2,3,4,5,6,1]
pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = toP (P.xrand e (map unP a) n)

-- * Non-SC3 patterns

-- | Type specialised 'P.fbool'.
pbool :: (Ord a,Num a) => P a -> P Bool
pbool = P.fbool

-- | 'mconcat' of 'replicate'.
pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = mconcat . replicate i

-- | Lifted 'P.countpost'.
pcountpost :: P Bool -> P Int
pcountpost = liftP P.countpost

-- | Lifted 'P.countpre'.
pcountpre :: P Bool -> P Int
pcountpre = liftP P.countpre

-- | Lifted 'P.hold'.
phold :: P a -> P a
phold = liftP P.hold

-- | Lifted 'P.interleave2'.
--
-- > let p = pinterleave2 (pwhite 'α' 1 9 inf) (pseries 10 1 5)
-- > in [3,10,9,11,2,12,9,13,4,14] `L.isPrefixOf` unP p
pinterleave2 :: P a -> P a -> P a
pinterleave2 = liftP2 P.interleave2

-- | Lifted 'P.interleave'.
--
-- > pinterleave [pwhitei 'α' 0 4 3,pwhitei 'β' 5 9 3] == toP [2,7,0,5,3,6]
pinterleave :: [P a] -> P a
pinterleave = toP . P.interleave . map unP

-- | Lifted 'L.isPrefixOf'.
pisPrefixOf :: Eq a => P a -> P a -> Bool
pisPrefixOf p q = L.isPrefixOf (unP p) (unP q)

-- | Lifted 'P.rsd'.
--
-- > prsd (pstutter 2 (toP [1,2,3])) == toP [1,2,3]
-- > prsd (pseq [1,2,3] 2) == toP [1,2,3,1,2,3]
prsd :: (Eq a) => P a -> P a
prsd = liftP P.rsd

-- | Lifted 'P.trigger'.
--
-- > let {tr = pbool (toP [0,1,0,0,1,1])
-- >     ;p = ptrigger tr (toP [1,2,3])
-- >     ;r = [Nothing,Just 1,Nothing,Nothing,Just 2,Just 3]}
-- > in p == toP r
ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing <> return (Just x)
    in join (pzipWith f r q)

-- * Event Patterns

-- | Synonym for a /field/ pattern.
type P_Field = P E.Field

-- | Synonym for an /event/ pattern.
type P_Event = P E.Event

-- | Synonym for ('E.Key','P_Field').
type P_Binding = (E.Key,P_Field)

-- | Synonym for ['P_Binding'], ie. 'pbind' input type.
type P_Bind = [P_Binding]

instance Audible P_Event where
    play = E.e_play . E.Event_Seq . unP

-- | Add a value to an existing key, or set the key if it doesn't exist.
--
-- > > p = Padd(\freq,801,Pbind(\freq,Pseq([100],1)));
-- > > p.asStream.all(()) == [('freq':901)]
--
-- > let p = padd (E.K_freq,801) (pbind [(E.K_freq,return 100)])
-- > in p == pbind [(E.K_freq,return 901)]
padd :: P_Binding -> P_Event -> P_Event
padd (k,p) = pzipWith (\i j -> E.e_edit k 0 (+ i) j) p

-- | Left-biased union of event patterns.
punion :: P_Event -> P_Event -> P_Event
punion = pzipWith (<>)

-- | SC3 pattern to assign keys to a set of value patterns making an
-- 'E.Event' pattern. A finite binding stops the 'E.Event' pattern.
--
-- > > p = Pbind(\x,Pseq([1,2,3],1),\y,Pseed(Pn(100,1),Prand([4,5,6],inf)));
-- > > p.asStream.all(()) == [('y':4,'x':1),('y':6,'x':2),('y':4,'x':3)]
--
-- > :set -XOverloadedStrings
--
-- > pkey "x" (pbind [("x",prand 'α' [100,300,200] inf)
-- >                 ,("y",pseq [1,2,3] 1)]) == toP [200,200,300]
--
-- > ptake 2 (pbind [("x",pwhitei 'α' 0 9 inf)
-- >                ,("y",pseq [1,2,3] inf)])
--
-- > > Pbind(\freq,Prand([300,500,231.2,399.2],inf),
-- > >       \dur,0.1).play;
--
-- > audition (pbind [(E.K_freq,prand 'a' [300,500,231.2,399.2] inf)
-- >                 ,(E.K_dur,0.1)])
--
-- > > Pbind(\freq, Prand([300,500,231.2,399.2],inf),
-- > >       \dur,Prand([0.1,0.3],inf)).play;
--
-- > audition (pbind [(E.K_freq,prand 'a' [300,500,231.2,399.2] inf)
-- >                 ,(E.K_dur,prand 'b' [0.1,0.3] inf)])
--
-- > > Pbind(\freq,Prand([1,1.2,2,2.5,3,4],inf) * 200,
-- > >       \dur,0.1).play;
--
-- > audition (pbind [(E.K_freq,prand 'a' [1,1.2,2,2.5,3,4] inf * 200)
-- >                 ,(E.K_dur,0.1)])
pbind :: P_Bind -> P_Event
pbind xs =
    let xs' = fmap (\(k,v) -> pzip (undecided k) v) xs
        xs'' = ptranspose_st_repeat xs'
    in fmap E.e_from_list xs''

-- | Two-channel MCE for /field/ patterns.
--
-- > pmce2 (toP [1,2]) (toP [3,4]) == toP [E.f_array [1,3],E.f_array [2,4]]
--
-- > let p = pmce2 (pseq [1,2] inf) (pseq [3,4] inf)
-- > in ptake 2 p == toP [E.f_array [1,3],E.f_array [2,4]]
pmce2 :: P_Field -> P_Field -> P_Field
pmce2 p = pzipWith (\m n -> E.F_Vector [m,n]) p

-- | Three-channel MCE for /field/ patterns.
pmce3 :: P_Field -> P_Field -> P_Field -> P_Field
pmce3 p q = pzipWith3 (\m n o -> E.F_Vector [m,n,o]) p q

-- | Edit 'a' at 'E.Key' in each element of an 'E.Event' pattern.
pedit :: E.Key -> (E.Field -> E.Field) -> P_Event -> P_Event
pedit k f = fmap (E.e_edit' k f)

-- | Pattern from 'I.Instr'.  An 'I.Instr' is either a 'Synthdef' or a
-- /name/.  In the 'Synthdef' case the instrument is asynchronously
-- sent to the server before processing the event, which has timing
-- implications.  The pattern constructed here uses the 'Synthdef' for
-- the first element, and the subsequently the /name/.
pinstr' :: I.Instr -> P_Field
pinstr' i = toP (map E.F_Instr (I.i_repeat i))

-- | 'I.Instr' pattern from instrument /name/.
pinstr :: String -> P_Field
pinstr s = pinstr' (I.Instr_Ref s True)

-- | 'I.Instr' pattern from 'Synthdef'.
psynth :: Synthdef -> P_Field
psynth s = pinstr' (I.Instr_Def s True)

-- | Pattern to extract 'a's at 'E.Key' from an 'E.Event'
-- pattern.
--
-- > pkey_m "freq" (pbind [("freq",440)]) == toP [Just 440]
pkey_m :: E.Key -> P_Event -> P (Maybe E.Field)
pkey_m k = fmap (E.e_get k)

-- | SC3 pattern to read 'a' of 'E.Key' at 'E.Event' pattern.
-- Note however that in haskell is usually more appropriate to name
-- the pattern using /let/.
--
-- > pkey "freq" (pbind [("freq",440)]) == toP [440]
-- > pkey "amp" (pbind [("amp",toP [0,1])]) == toP [0,1]
pkey :: E.Key -> P_Event -> P_Field
pkey k = fmap (fromJust . E.e_get k)

-- | SC3 pattern that is a variant of 'pbind' for controlling
-- monophonic (persistent) synthesiser nodes.
pmono :: P_Bind -> P_Event
pmono b =
    let ty = fmap E.F_String ("s_new" `pcons` prepeat "n_set")
    in pbind ((E.K_type,ty) : b)

-- | Idiom to scale 'a' at 'E.Key' in an 'E.Event' pattern.
pmul :: P_Binding -> P_Event -> P_Event
pmul (k,p) = pzipWith (\i j -> E.e_edit k 1 (* i) j) p

-- | Variant that does not insert key.
pmul' :: P_Binding -> P_Event -> P_Event
pmul' (k,p) = pzipWith (\i j -> E.e_edit' k (* i) j) p

-- | SC3 pattern to do time stretching.  It is equal to 'pmul' at
-- 'E.K_stretch'.
pstretch :: P_Field -> P_Event -> P_Event
pstretch p = pmul (E.K_stretch,p)

-- | 'punion' of 'pbind' of 'return', ie. @p_with (K_Instr,psynth s)@.
p_with :: P_Binding -> P_Event -> P_Event
p_with = punion . pbind . return

-- * Parallel patterns

-- | Merge two 'E.Event' patterns with indicated start 'Time's.
ptmerge :: (Time,P_Event) -> (Time,P_Event) -> P_Event
ptmerge (pt,p) (qt,q) =
    toP (E.e_merge (pt,F.toList p) (qt,F.toList q))

-- | Variant of 'ptmerge' with zero start times.
pmerge :: P_Event -> P_Event -> P_Event
pmerge p q = ptmerge (0,p) (0,q)

-- | Merge a set of 'E.Event' patterns each with indicated start 'Time'.
ptpar :: [(Time,P_Event)] -> P_Event
ptpar l =
    case l of
      [] -> mempty
      [(_,p)] -> p
      (pt,p):(qt,q):r -> ptpar ((min pt qt,ptmerge (pt,p) (qt,q)) : r)

-- | Variant of 'ptpar' with zero start times.
--
-- > let {a = pbind [("a",pseq [1,2,3] inf)]
-- >     ;b = pbind [("b",pseq [4,5,6] inf)]
-- >     ;r = toP [E.e_from_list [("a",1),("fwd'",0)]
-- >              ,E.e_from_list [("b",4),("fwd'",1)]]}
-- > in ptake 2 (ppar [a,b]) == r
ppar :: [P_Event] -> P_Event
ppar l = ptpar (zip (repeat 0) l)

-- * MCE

-- | Remove one layer of MCE expansion at an /event/ pattern.  The
-- pattern will be expanded only to the width of the initial input.
-- Holes are filled with rests.
--
-- > let {a = pseq [65,69,74] inf
-- >     ;b = pseq [60,64,67,72,76] inf
-- >     ;c = pseq [pmce3 72 76 79,pmce2 a b] 1}
-- > in audition (p_un_mce (pbind [("midinote",c)
-- >                              ,("pan",pmce2 (-1) 1)
-- >                              ,("dur",1 `pcons` prepeat 0.15)]))
p_un_mce :: P_Event -> P_Event
p_un_mce p =
    let l' = P.transpose_fw_def' E.e_rest (map E.e_un_mce' (unP p))
    in toP (E.e_par (zip (repeat 0) l'))

-- * Aliases

-- | Type specialised 'mappend'.
pappend :: P a -> P a -> P a
pappend = mappend

-- | Type specialised 'mconcat'.
pconcat :: [P a] -> P a
pconcat = mconcat

-- | Type specialised `mempty`.
pempty :: P a
pempty = mempty

-- | Type specialised 'join'.
pjoin :: P (P a) -> P a
pjoin = join

-- | 'undecided' preserving pattern 'join'.
--
-- > join (undecided (undecided 1)) == undecided 1
-- > ptake 3 (join (undecided (return 1))) == toP [1]
-- > ptake 3 (pjoin_repeat (undecided (return 1))) == toP [1,1,1]
pjoin_repeat :: P (P a) -> P a
pjoin_repeat p =
    case p of
      P (Left (P (Right l))) -> P (Right (cycle l))
      _ -> join p

-- | Type specialised 'fmap'.
pmap :: (a -> b) -> P a -> P b
pmap = fmap

-- | Type specialised 'pure'.
ppure :: a -> P a
ppure = pure

-- | Type specialised 'return'
preturn :: a -> P a
preturn = return

-- * NRT

-- | Transform an /event/ pattern into a /non-real time/ SC3 score.
pNRT :: P_Event -> NRT
pNRT = E.e_nrt . E.Event_Seq . unP

-- * UId variants

-- | 'liftUId' of 'pbrown'.
pbrownM :: (UId m,Num n,Ord n,Random n) => n -> n -> n -> Int -> m (P n)
pbrownM = liftUId4 pbrown

-- | 'liftUId' of 'pexprand'.
pexprandM :: (UId m,Random a,Floating a) => a -> a -> Int -> m (P a)
pexprandM = liftUId3 pexprand

-- | 'liftUId' of 'prand'.
prandM :: UId m => [P a] -> Int -> m (P a)
prandM = liftUId2 prand

-- | 'liftUId' of 'pshuf'.
pshufM :: UId m => [a] -> Int -> m (P a)
pshufM = liftUId2 pshuf

-- | 'liftUId' of 'pwhite'.
pwhiteM :: (UId m,Random n) => n -> n -> Int -> m (P n)
pwhiteM = liftUId3 pwhite

-- | 'liftUId' of 'pwhitei'.
pwhiteiM :: (UId m,RealFracE n,Random n) => n -> n -> Int -> m (P n)
pwhiteiM = liftUId3 pwhitei

-- | 'liftUId' of 'pwrand'.
pwrandM :: UId m => [P a] -> [Double] -> Int -> m (P a)
pwrandM = liftUId3 pwrand

-- | 'liftUId' of 'pxrand'.
pxrandM :: UId m => [P a] -> Int -> m (P a)
pxrandM = liftUId2 pxrand
