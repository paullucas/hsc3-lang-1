-- | @sclang@ value pattern functions.
-- See <http://rd.slavepianos.org/?t=hsc3-texts> for tutorial.
--
-- SC3 /value/ patterns: `pbrown` (Pbrown), `pclutch` (Pclutch),
-- `pcollect` (Pcollect), `pconst` (Pconst), `pdegreeToKey`
-- (PdegreeToKey), `pdiff` (Pdiff), `pdrop` (Pdrop), `pdurStutter`
-- (PdurStutter), `pexprand` (Pexprand), `pfinval` (Pfinval), `pfuncn`
-- (Pfuncn), `pgeom` (Pgeom), `pif` (Pif), `place` (Place), `pn` (Pn),
-- `ppatlace` (Ppatlace), `prand` (Prand), `preject` (Preject),
-- `prorate` (Prorate), `pselect` (Pselect), `pseq` (Pseq), `pser`
-- (Pser), `pseries` (Pseries), `pshuf` (Pshuf), `pslide` (Pslide),
-- `pstutter` (Pstutter), `pswitch1` (Pswitch1), `pswitch` (Pswitch),
-- `ptuple` (Ptuple), `pwhite` (Pwhite), `pwrand` (Pwrand), `pwrap`
-- (Pwrap), `pxrand` (Pxrand).
--
-- SC3 variant patterns: `pbrown`', `prand'`, `prorate'`, `pseq1`,
-- `pseqn`, `pser1`, `pseqr`, `pwhite'`, `pwhitei`.
--
-- SC3 collection patterns: `pfold`
--
-- Haskell patterns: `pappend`, `pbool`, `pconcat`, `pcons`,
-- `pcountpost`, `pcountpre`, `pcycle`, `pempty`,`pfilter`, `phold`,
-- `pinterleave`,`pjoin`, `prepeat`, `preplicate`, `prsd`, `pscanl`,
-- `psplitPlaces`, `psplitPlaces'`, `ptail`, `ptake`, `ptrigger`,
-- `pzip`, `pzipWith`
module Sound.SC3.Lang.Pattern.ID where

import Control.Applicative hiding ((<*)) {- base -}
import Control.Monad {- base -}
import Data.Bifunctor {- bifunctors -}
import qualified Data.Foldable as F {- base -}
import qualified Data.List as L {- base -}
import qualified Data.List.Split as S {- split -}
import Data.Monoid {- base -}
import qualified Data.Traversable as T {- base -}
import Sound.SC3 {- hsc3 -}
import System.Random {- random -}

import qualified Sound.SC3.Lang.Collection as C
import Sound.SC3.Lang.Core
import qualified Sound.SC3.Lang.Math as M
import qualified Sound.SC3.Lang.Pattern.List as P
import qualified Sound.SC3.Lang.Pattern.Stream as I
import qualified Sound.SC3.Lang.Random.Gen as R

-- * P

-- | Patterns are opaque.  @P a@ is a pattern with elements of type
-- @a@.  Patterns are constructed, manipulated and destructured using
-- the functions provided, ie. the pattern instances for 'return',
-- 'pure' and 'F.toList', and the pattern specific functions
-- 'undecided' and 'toP'.
--
-- > F.toList (toP [1,2,3] * 2) == [2,4,6]
--
-- Patterns are 'Functor's.  'fmap' applies a function to each element
-- of a pattern.
--
-- > fmap (* 2) (toP [1,2,3,4,5]) == toP [2,4,6,8,10]
--
-- Patterns are 'Monoid's.  'mempty' is the empty pattern, and
-- 'mappend' ('<>') makes a sequence of two patterns.
--
-- > 1 <> mempty <> 2 == toP [1,2]
--
-- Patterns are 'Applicative'.  The pattern instance is pointwise &
-- truncating, as for 'ZipList'.  'pure' lifts a value into an
-- infinite pattern of itself, '<*>' applies a pattern of functions to
-- a pattern of values.  This is distinct from the combinatorial
-- instance for ordinary lists, ie. where 'pure' is 'return' and '<*>'
-- is 'ap'.
--
-- > liftA2 (+) (toP [1,2]) (toP [3,4,5]) == toP [4,6]
-- > liftA2 (+) [1,2] [3,4,5] == [4,5,6,5,6,7]
--
-- Patterns are 'Monad's, and therefore allow /do/ notation.
--
-- > let p = do {x <- toP [1,2]; y <- toP [3,4,5]; return (x,y)}
-- > in p == toP [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
--
-- Patterns are 'Num'erical.  The instances can be derived from the
-- 'Applicative' instance.
--
-- > 1 + toP [2,3,4] == liftA2 (+) 1 (toP [2,3,4])
data P a = P {unP_either :: Either a [a]}
           deriving (Eq,Show)

-- | Lift a value to a pattern deferring deciding if the constructor
-- ought to be 'pure' or 'return' to the consuming function.  The
-- pattern instances for 'fromInteger' and 'fromRational' make
-- 'undecided' patterns.  In general /horizontal/ functions (ie. '<>')
-- resolve using 'return' and /vertical/ functions (ie. 'zip') resolve
-- using 'pure'.  In the documentation functions that resolve using
-- 'pure' are annotated as /implicitly repeating/.
--
-- > 1 <> toP [2,3] == return 1 <> toP [2,3]
-- > toP [1,2] * 3  == toP [1,2] * pure 3
undecided :: a -> P a
undecided a = P (Left a)

{- | The basic list to pattern function, inverse is 'unP'.

> unP (toP "str") == "str"

There is a @default@ sound, given by 'defaultSynthdef' from "Sound.SC3".

> audition defaultSynthdef

If no instrument is specified we hear the default.

> audition (pbind [(K_degree,pxrand 'α' [0,1,5,7] inf)
>                 ,(K_dur,toP [0.1,0.2,0.1])])

> > Pbind(\degree,(Pxrand([0,1,5,7],inf))
> >      ,\dur,Pseq([0.1,0.2,0.1],1)).play

The pattern above is finite, `toP` can sometimes be replaced with
`pseq`.

> audition (pbind [(K_degree,pxrand 'α' [0,1,5,7] inf)
>                 ,(K_dur,pseq [0.1,0.2,0.1] inf)])

-}
toP :: [a] -> P a
toP = P . Right

-- | Type specialised 'F.toList'.  'undecided' values are singular.
--
-- > F.toList (undecided 'a') == ['a']
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

instance T.Traversable P where
    traverse f p = pure toP <*> T.traverse f (unP p)

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
    (>) = error ("~> Ord>*")
    (>=) = error ("~> Ord>=*")
    (<) = error ("~> Ord<*")
    (<=) = error ("~> Ord<=*")

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
--
-- > liftP2 (zipWith (+)) (toP [1,2]) (toP [3,4,5]) == toP [4,6]
-- > liftA2 (+) (toP [1,2]) (toP [3,4,5]) == toP [4,6]
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

{-| Constant /NaN/ (not a number) value.

> isNaN nan == True

A frequency value of NaN indicates a rest. This constant value can be
used as a rest indicator at a frequency model input (not at a @rest@
key).

> audition (pbind [(K_dur,pseq [0.1,0.7] inf)
>                 ,(K_legato,0.2)
>                 ,(K_degree,pseq [0,2,return nan] inf)])

-}
nan :: Floating a => a
nan = sqrt (-1)

-- * Data.List Patterns

-- | Pattern variant of 'Data.List.:'.
--
-- > pcons 'α' (pn (return 'β') 2) == toP "αββ"
pcons :: a -> P a -> P a
pcons = mappend . return

-- | Pattern variant of 'Data.List.null'.
--
-- > pnull mempty == True
-- > pnull (undecided 'a') == False
-- > pnull (pure 'a') == False
-- > pnull (return 'a') == False
pnull :: P a -> Bool
pnull = null . unP

-- | Alias for 'pure', pattern variant of 'Data.List.repeat'.
--
-- > ptake 5 (prepeat 3) == toP [3,3,3,3,3]
-- > ptake 5 (pure 3) == toP [3,3,3,3,3]
-- > take 5 (pure 3) == [3]
prepeat :: a -> P a
prepeat = pure

-- | Pattern variant of 'splitAt'.
psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p =
    let (i,j) = splitAt n (unP p)
    in (toP i,toP j)

-- | Pattern variant of 'Data.List.Split.splitPlaces'.
--
-- > psplitPlaces' (toP [1,2,3]) (pseries 1 1 6) == toP [[1],[2,3],[4,5,6]]
-- > psplitPlaces' (toP [1,2,3]) (toP ['a'..]) == toP ["a","bc","def"]
psplitPlaces' :: P Int -> P a -> P [a]
psplitPlaces' = liftP2 S.splitPlaces

-- | 'fmap' 'toP' of 'psplitPlaces''.
--
-- > psplitPlaces (toP [1,2,3]) (toP ['a'..]) == toP (map toP ["a","bc","def"])
psplitPlaces :: P Int -> P a -> P (P a)
psplitPlaces = fmap toP .: psplitPlaces'

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

-- | Type specialised 'mfilter'.  Aliased to `pselect`.  See also
-- `preject`.
--
-- > mfilter even (pseq [1,2,3] 2) == toP [2,2]
-- > mfilter (< 3) (pseq [1,2,3] 2) == toP [1,2,1,2]
pfilter :: (a -> Bool) -> P a -> P a
pfilter = mfilter

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
-- returns a list of successive reduced values from the left.  pscanl
-- is an accumulator, it provides a mechanism for state to be threaded
-- through a pattern. It can be used to write a function to remove
-- succesive duplicates from a pattern, to count the distance between
-- occurences of an element in a pattern and so on.
--
-- > F.foldl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == 43
-- > pscanl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == toP [4,9,20,43]
--
-- > F.foldl (flip (:)) [] (toP [1..3]) == [3,2,1]
-- > pscanl (flip (:)) [] (toP [1..3]) == toP [[],[1],[2,1],[3,2,1]]
--
-- > F.foldl (+) 0 (toP [1..5]) == 15
-- > pscanl (+) 0 (toP [1..5]) == toP [0,1,3,6,10,15]
pscanl :: (a -> b -> a) -> a -> P b -> P a
pscanl f i = liftP (L.scanl f i)

-- | 'pdrop' @1@.  Pattern variant of `Data.List.tail`.  Drops first
-- element from pattern.  Note that the haskell `tail` function is
-- partial, although `drop` is not.  `ptake` is equal to `pdrop 1`.
--
-- > tail [] == _|_
-- > drop 1 [] == []
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

-- * SC3 Collection Patterns

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

{- | Type specialised 'P.ffold'.

> pfold (toP [10,11,12,-6,-7,-8]) (-7) 11 == toP [10,11,10,-6,-7,-6]

> audition (pbind [(K_degree,pfold (pseries 4 1 inf) (-7) 11)
>                 ,(K_dur,0.0625)])

The underlying primitive is then `fold_` function.

> let f = fmap (\n -> fold_ n (-7) 11)
> in audition (pbind [(K_degree,f (pseries 4 1 inf))
>                    ,(K_dur,0.0625)])

-}
pfold :: (RealFrac n) => P n -> n -> n -> P n
pfold = P.ffold

-- | Pattern variant of 'C.normalizeSum'.
pnormalizeSum :: Fractional n => P n -> P n
pnormalizeSum = liftP C.normalizeSum

-- * SC3 Patterns

{-| Pbrown.  Lifted 'P.brown'.  SC3 pattern to generate
psuedo-brownian motion.

> pbrown 'α' 0 9 1 5 == toP [4,4,5,4,3]

> audition (pbind [(K_dur,0.065)
>                 ,(K_freq,pbrown 'α' 440 880 20 inf)])

-}
pbrown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> Int -> P n
pbrown e l r s n = ptake n (toP (P.brown e l r s))

{-| Pclutch.  SC3 sample and hold pattern.  For true values in the
control pattern, step the value pattern, else hold the previous value.

> > c = Pseq([1,0,1,0,0,1,1],inf);
> > p = Pclutch(Pser([1,2,3,4,5],8),c);
> > r = [1,1,2,2,2,3,4,5,5,1,1,1,2,3];
> > p.asStream.all == r

> let {c = pbool (pseq [1,0,1,0,0,1,1] inf)
>     ;p = pclutch (pser [1,2,3,4,5] 8) c
>     ;r = toP [1,1,2,2,2,3,4,5,5,1,1,1,2,3]}
> in p == toP [1,1,2,2,2,3,4,5,5,1,1,1,2,3]

Note the initialization behavior, nothing is generated until the
first true value.

> let {p = pseq [1,2,3,4,5] 1
>     ;q = pbool (pseq [0,0,0,0,0,0,1,0,0,1,0,1] 1)}
> in pclutch p q == toP [1,1,1,2,2,3]

> > Pbind(\degree,Pstutter(Pwhite(3,10,inf),Pwhite(-4,11,inf)),
> >       \dur,Pclutch(Pwhite(0.1,0.4,inf),
> >                    Pdiff(Pkey(\degree)).abs > 0),
> >       \legato,0.3).play;

> let {d = pstutter (pwhite 'α' 3 10 inf) (pwhitei 'β' (-4) 11 inf)
>     ;p = [(K_degree,d)
>          ,(K_dur,pclutch (pwhite 'γ' 0.1 0.4 inf)
>                          (pbool (abs (pdiff d) >* 0)))
>          ,(K_legato,0.3)]}
> in audition (pbind p)

-}
pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

-- | Pcollect.  SC3 name for 'fmap', ie. patterns are functors.
--
-- > > Pcollect({|i| i * 3},Pseq(#[1,2,3],1)).asStream.all == [3,6,9]
-- > pcollect (* 3) (toP [1,2,3]) == toP [3,6,9]
--
-- > > Pseq(#[1,2,3],1).collect({|i| i * 3}).asStream.all == [3,6,9]
-- > fmap (* 3) (toP [1,2,3]) == toP [3,6,9]
pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

{- | Pconst.  SC3 pattern to constrain the sum of a numerical pattern.
Is equal to /p/ until the accumulated sum is within /t/ of /n/.  At
that point, the difference between the specified sum and the
accumulated sum concludes the pattern.

> > p = Pconst(10,Pseed(Pn(1000,1),Prand([1,2,0.5,0.1],inf),0.001));
> > p.asStream.all == [0.5,0.1,0.5,1,2,2,0.5,1,0.5,1,0.9]

> let p = pconst 10 (prand 'α' [1,2,0.5,0.1] inf) 0.001
> in (p,Data.Foldable.sum p)

> > Pbind(\degree,Pseq([-7,Pwhite(0,11,inf)],1),
> >       \dur,Pconst(4,Pwhite(1,4,inf) * 0.25)).play

> let p = [(K_degree,pcons (-7) (pwhitei 'α' 0 11 inf))
>         ,(K_dur,pconst 4 (pwhite 'β' 1 4 inf * 0.25) 0.001)]
> in audition (pbind p)

-}
pconst :: (Ord a,Num a) => a -> P a -> a -> P a
pconst n p t =
    let f _ [] = []
        f j (i:is) = if i + j < n - t
                     then i : f (j + i) is
                     else [n - j]
    in toP (f 0 (unP p))

{-| PdegreeToKey.  SC3 pattern to derive notes from an index into a
scale.

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = pure [0,2,4,5,7,9,11]
>     ;r = [0,2,4,5,7,5,4,2,0,4,7,12,7,4,0,2,4,5,7,5,4,2,0,4,7,12,7,4]}
> in pdegreeToKey p q (pure 12) == toP r

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = pseq (map return [[0,2,4,5,7,9,11],[0,2,3,5,7,8,11]]) 1
>     ;r = [0,2,4,5,7,5,4,2,0,4,7,12,7,4,0,2,3,5,7,5,3,2,0,3,7,12,7,3]}
> in pdegreeToKey p (pstutter 14 q) (pure 12) == toP r

This is the pattern variant of 'M.degreeToKey'.

> let s = [0,2,4,5,7,9,11]
> in map (M.degreeToKey s 12) [0,2,4,7,4,2,0] == [0,4,7,12,7,4,0]

> > Pbind(\note,PdegreeToKey(Pseq([1,2,3,2,5,4,3,4,2,1],2),
> >                          #[0,2,3,6,7,9],
> >                          12),\dur,0.25).play

> let {n = pdegreeToKey (pseq [1,2,3,2,5,4,3,4,2,1] 2)
>                       (pure [0,2,3,6,7,9])
>                       12}
> in audition (pbind [(K_note,n),(K_dur,0.25)])

> > s = #[[0,2,3,6,7,9],[0,1,5,6,7,9,11],[0,2,3]];
> > d = [1,2,3,2,5,4,3,4,2,1];
> > Pbind(\note,PdegreeToKey(Pseq(d,4),
> >                          Pstutter(3,Prand(s,inf)),
> >                          12),\dur,0.25).play;

> let {s = map return [[0,2,3,6,7,9],[0,1,5,6,7,9,11],[0,2,3]]
>     ;d = [1,2,3,2,5,4,3,4,2,1]
>     ;k = pdegreeToKey (pseq d 4)
>                       (pstutter 3 (prand 'α' s 14))
>                       (pn 12 40)}
> in audition (pbind [(K_note,k),(K_dur,0.25)])

-}
pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 (\i j k -> M.degreeToKey j k i)

-- | Pdiff.  SC3 pattern to calculate adjacent element difference.
--
-- > > Pdiff(Pseq([0,2,3,5,6,8,9],1)).asStream.all == [2,1,2,1,2,1]
-- > pdiff (pseq [0,2,3,5,6,8,9] 1) == toP [2,1,2,1,2,1]
pdiff :: Num n => P n -> P n
pdiff p = ptail p - p

-- | Pdrop.  Lifted 'L.drop'.
--
-- > > p = Pseries(1,1,20).drop(5);
-- > > p.asStream.all == [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--
-- > pdrop 5 (pseries 1 1 10) == toP [6,7,8,9,10]
-- > pdrop 1 mempty == mempty
pdrop :: Int -> P a -> P a
pdrop n = liftP (drop n)

{- | PdurStutter.  Lifted 'P.durStutter'.

> > s = Pseq(#[1,1,1,1,1,2,2,2,2,2,0,1,3,4,0],inf);
> > d = Pseq(#[0.5,1,2,0.25,0.25],1);
> > PdurStutter(s,d).asStream.all == [0.5,1,2,0.25,0.25]

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] 1}
> in pdurStutter s d == toP [0.5,1.0,2.0,0.25,0.25]

Applied to duration.

> > d = PdurStutter(Pseq(#[1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4],inf),
> >                 Pseq(#[0.5,1,2,0.25,0.25],inf));
> > Pbind(\freq,440,\dur,d).play

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] inf}
> in audition (pbind [(K_freq,440),(K_dur,pdurStutter s d)])

Applied to frequency.

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,0,4,4] inf
>     ;d = pseq [0,2,3,5,7,9,10] inf + 80}
> in audition (pbind [(K_midinote,pdurStutter s d),(K_dur,0.15)])

-}
pdurStutter :: Fractional a => P Int -> P a -> P a
pdurStutter = liftP2 P.durStutter

-- | Pexprand.  Lifted 'P.exprand'.
--
-- > > Pexprand(0.0001,1,10).asStream.all
-- > pexprand 'α' 0.0001 1 10
--
-- > > Pbind(\freq,Pexprand(0.0001,1,inf) * 600 + 300,\dur,0.02).play
--
-- > audition (pbind [(K_freq,pexprand 'α' 0.0001 1 inf * 600 + 300)
-- >                 ,(K_dur,0.02)])
pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r = toP . P.exprand e l r

-- | Pfinval.  Alias for 'ptake'
--
-- > > Pfinval(5,Pseq(#[1,2,3],inf)).asStream.all == [1,2,3,1,2]
-- > pfinval 5 (pseq [1,2,3] inf) == toP [1,2,3,1,2]
pfinval :: Int -> P a -> P a
pfinval = ptake

{-|
A variant of the SC3 pattern that evaluates a closure at each
step.  The haskell variant function has a 'StdGen' form.

> > p = Pfuncn({exprand(0.1,0.3) + #[1,2,3,6,7].choose},inf);
> > Pbind(\freq,p * 100 + 300,\dur,0.02).play

> let {exprand = Sound.SC3.Lang.Random.Gen.exprand
>     ;choose = Sound.SC3.Lang.Random.Gen.choose
>     ;p = pfuncn 'α' (exprand 0.1 0.3) inf
>     ;q = pfuncn 'β' (choose [1,2,3,6,7]) inf}
> in audition (pbind [(K_freq,(p + q) * 100 + 300),(K_dur,0.02)])

Of course in this case there is a pattern equivalent.

> let {p = pexprand 'α' 0.1 0.3 inf + prand 'β' [1,2,3,6,7] inf}
> in audition (pbind [(K_freq,p * 100 + 300),(K_dur,0.02)])

-}
pfuncn :: Enum e => e -> (StdGen -> (n,StdGen)) -> Int -> P n
pfuncn e f n = toP (P.funcn e f n)

{- | Pgeom.  SC3 geometric series pattern.

> > Pgeom(3,6,5).asStream.all == [3,18,108,648,3888]
> pgeom 3 6 5 == toP [3,18,108,648,3888]

> > Pgeom(1,2,10).asStream.all == [1,2,4,8,16,32,64,128,256,512]
> pgeom 1 2 10 == toP [1,2,4,8,16,32,64,128,256,512]

Real numbers work as well.

> > p = Pgeom(1.0,1.1,6).collect({|i| (i * 100).floor});
> > p.asStream.all == [100,110,121,133,146,161];

> let p = fmap (floor . (* 100)) (pgeom 1.0 1.1 6)
> in p == toP [100,110,121,133,146,161]

> > Pbind(\degree,Pseries(-7,1,15),
> >       \dur,Pgeom(0.5,0.89140193218427,15)).play;

> audition (pbind [(K_degree,pseries (-7) 1 15)
>                 ,(K_dur,pgeom 0.5 0.89140193218427 15)])

There is a list variant.

> > 5.geom(3,6)
> C.geom 5 3 6 == [3,18,108,648,3888]

-}
pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = toP (C.geom n i s)

-- | Pif.  SC3 /implicitly repeating/ pattern-based conditional expression.
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

-- | Place.  SC3 interlaced embedding of subarrays.
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

-- | Pn.  SC3 pattern to repeat the enclosed pattern a number of
-- times.
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

{- | Ppatlace.  SC3 /implicitly repeating/ pattern to lace input patterns.

> > p = Ppatlace([1,Pseq([2,3],2),4],5);
> > p.asStream.all == [1,2,4,1,3,4,1,2,4,1,3,4,1,4]

> ppatlace [1,pseq [2,3] 2,4] 5 == toP [1,2,4,1,3,4,1,2,4,1,3,4,1,4]

> > p = Ppatlace([1,Pseed(Pn(1000,1),Prand([2,3],inf))],5);
> > p.asStream.all == [1,3,1,3,1,3,1,2,1,2]

> ppatlace [1,prand 'α' [2,3] inf] 5 == toP [1,3,1,2,1,3,1,2,1,2]

> > Pbind(\degree,Ppatlace([Pseries(0,1,8),Pseries(2,1,7)],inf),
> >       \dur,0.25).play;

> let p = [(K_degree,ppatlace [pseries 0 1 8,pseries 2 1 7] inf)
>         ,(K_dur,0.125)]
> in audition (pbind p)

-}
ppatlace :: [P a] -> Int -> P a
ppatlace a n =
    let a' = L.transpose (map unP_repeat a)
    in toP (L.concat (P.take_inf n a'))

{-| Prand.  SC3 pattern to make n random selections from a list of
patterns, the resulting pattern is flattened (joined).

> > p = Pseed(Pn(1000,1),Prand([1,Pseq([10,20,30]),2,3,4,5],6));
> > p.asStream.all == [3,5,3,10,20,30,2,2]

> prand 'α' [1,toP [10,20],2,3,4,5] 5 == toP [5,2,10,20,2,1]

> > Pbind(\note,Prand([0,1,5,7],inf),\dur,0.25).play

> audition (pbind [(K_note,prand 'α' [0,1,5,7] inf),(K_dur,0.25)])

Nested sequences of pitches:

> > Pbind(\midinote,Prand([Pseq(#[60,61,63,65,67,63]),
> >                        Prand(#[72,73,75,77,79],6),
> >                        Pshuf(#[48,53,55,58],2)],inf),
> >       \dur,0.25).play

> let {n = prand 'α' [pseq [60,61,63,65,67,63] 1
>                    ,prand 'β' [72,73,75,77,79] 6
>                    ,pshuf 'γ' [48,53,55,58] 2] inf}
> in audition (pbind [(K_midinote,n),(K_dur,0.075)])

The below cannot be written as intended with the list
based pattern library.  This is precisely because the
noise patterns are values, not processes with a state
threaded non-locally.

> do {n0 <- Sound.SC3.Lang.Random.IO.rrand 2 5
>    ;n1 <- Sound.SC3.Lang.Random.IO.rrand 3 9
>    ;let p = pseq [prand 'α' [pempty,pseq [24,31,36,43,48,55] 1] 1
>                  ,pseq [60,prand 'β' [63,65] 1
>                        ,67,prand 'γ' [70,72,74] 1] n0
>                  ,prand 'δ' [74,75,77,79,81] n1] inf
>     in return (ptake 24 p)}

-}
prand :: Enum e => e -> [P a] -> Int -> P a
prand = join .:: prand'

-- | Preject.  SC3 pattern to rejects values for which the predicate
-- is true.  reject f is equal to filter (not . f).
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

{- | Prorate.  SC3 /implicitly repeating/ sub-dividing pattern.

> > p = Prorate(Pseq([0.35,0.5,0.8]),1);
> > p.asStream.all == [0.35,0.65,0.5,0.5,0.8,0.2];

> let p = prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1
> in fmap roundE (p * 100) == toP [35,65,50,50,80,20]

> > p = Prorate(Pseq([0.35,0.5,0.8]),Pseed(Pn(100,1),Prand([20,1],inf)));
> > p.asStream.all == [7,13,0.5,0.5,16,4]

> let p = prorate (fmap Left (pseq [0.35,0.5,0.8] 1))
>                 (prand 'α' [20,1] 3)
> in fmap roundE (p * 100) == toP [35,65,1000,1000,80,20]

> > l = [[1,2],[5,7],[4,8,9]].collect(_.normalizeSum);
> > Prorate(Pseq(l,1)).asStream.all

> let l = map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]]
> in prorate (toP l) 1

> > Pfinval(5,Prorate(0.6,0.5)).asStream.all == [0.3,0.2,0.3,0.2,0.3]

> pfinval 5 (prorate (fmap Left 0.6) 0.5) == toP [0.3,0.2,0.3,0.2,0.3]

> > Pbind(\degree,Pseries(4,1,inf).fold(-7,11),
> >       \dur,Prorate(0.6,0.5)).play

> audition (pbind [(K_degree,pfold (pseries 4 1 inf) (-7) 11)
>                 ,(K_dur,prorate (fmap Left 0.6) 0.25)])

-}
prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate = pjoin_repeat .: pzipWith prorate'

-- | Pselect.  See 'pfilter'.
--
-- > pselect (< 3) (pseq [1,2,3] 2) == toP [1,2,1,2]
pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

{-| Pseq.  SC3 pattern to cycle over a list of patterns. The repeats
pattern gives the number of times to repeat the entire list.

> pseq [return 1,return 2,return 3] 2 == toP [1,2,3,1,2,3]
> pseq [1,2,3] 2 == toP [1,2,3,1,2,3]
> pseq [1,pn 2 2,3] 2 == toP [1,2,2,3,1,2,2,3]

There is an 'inf' value for the repeats variable.

> ptake 3 (pdrop (10^5) (pseq [1,2,3] inf)) == toP [2,3,1]

Unlike the SC3 Pseq, `pseq` does not have an offset argument to give a
starting offset into the list.

> pseq (C.rotate 3 [1,2,3,4]) 3 == toP [2,3,4,1,2,3,4,1,2,3,4,1]

As scale degrees.

> > Pbind(\degree,Pseq(#[0,0,4,4,5,5,4],1),
> >       \dur,Pseq(#[0.5,0.5,0.5,0.5,0.5,0.5,1],1)).play

> audition (pbind [(K_degree,pseq [0,0,4,4,5,5,4] 1)
>                 ,(K_dur,pseq [0.5,0.5,0.5,0.5,0.5,0.5,1] 1)])

> > Pseq(#[60,62,63,65,67,63],inf) + Pseq(#[0,0,0,0,-12],inf)

> let n = pseq [60,62,63,65,67,63] inf + pser [0,0,0,0,-12] 25
> in audition (pbind [(K_midinote,n),(K_dur,0.2)])

Pattern `b` pattern sequences `a` once normally, once transposed up a
fifth and once transposed up a fourth.

> > a = Pseq(#[60,62,63,65,67,63]);
> > b = Pseq([a,a + 7,a + 5],inf);
> > Pbind(\midinote,b,\dur,0.3).play

> let {a = pseq [60,62,63,65,67,63] 1
>     ;b = pseq [a,a + 7,a + 5] inf}
> in audition (pbind [(K_midinote,b),(K_dur,0.13)])

-}
pseq :: [P a] -> Int -> P a
pseq a i =
    let a' = mconcat a
    in if i == inf then pcycle a' else pn a' i

-- | Pser.  SC3 pattern that is like 'pseq', however the repeats
-- variable gives the number of elements in the sequence, not the
-- number of cycles of the pattern.
--
-- > pser [1,2,3] 5 == toP [1,2,3,1,2]
-- > pser [1,pser [10,20] 3,3] 9 == toP [1,10,20,10,3,1,10,20,10]
-- > pser [1,2,3] 5 * 3 == toP [3,6,9,3,6]
pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (mconcat a))

-- | Pseries.  SC3 arithmetric series pattern, see also 'pgeom'.
--
-- > pseries 0 2 10 == toP [0,2,4,6,8,10,12,14,16,18]
-- > pseries 9 (-1) 10 == toP [9,8 .. 0]
-- > pseries 1.0 0.2 3 == toP [1.0::Double,1.2,1.4]
pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = toP (C.series n i s)

{- | Pshuf.  SC3 pattern to return @n@ repetitions of a shuffled
-- sequence.

> > Pshuf([1,2,3,4],2).asStream.all
> pshuf 'α' [1,2,3,4] 2 == toP [2,4,3,1,2,4,3,1]

> > Pbind(\degree,Pshuf([0,1,2,4,5],inf),\dur,0.25).play

> audition (pbind [(K_degree,pshuf 'α' [0,1,2,4,5] inf)
>                 ,(K_dur,0.25)])

-}
pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a =
    let (a',_) = R.scramble a (mkStdGen (fromEnum e))
    in pn (toP a')

{- | Pslide.  Lifted 'P.slide'.

> > Pslide([1,2,3,4],inf,3,1,0).asStream.all
> pslide [1,2,3,4] 4 3 1 0 True == toP [1,2,3,2,3,4,3,4,1,4,1,2]
> pslide [1,2,3,4,5] 3 3 (-1) 0 True == toP [1,2,3,5,1,2,4,5,1]

> > Pbind(\degree,Pslide((-6,-4 .. 12),8,3,1,0),
> >       \dur,Pseq(#[0.1,0.1,0.2],inf),
> >       \sustain,0.15).play

> audition (pbind [(K_degree,pslide [-6,-4 .. 12] 8 3 1 0 True)
>                 ,(K_dur,pseq [0.05,0.05,0.1] inf)
>                 ,(K_sustain,0.15)])

-}
pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide = toP .::::: P.slide

{- | Pstutter.  SC3 /implicitly repeating/ pattern to repeat each
-- element of a pattern /n/ times.

> > Pstutter(2,Pseq([1,2,3],1)).asStream.all == [1,1,2,2,3,3]
> pstutter 2 (pseq [1,2,3] 1) == toP [1,1,2,2,3,3]

The count input may be a pattern.

> let {p = pseq [1,2] inf
>     ;q = pseq [1,2,3] 2}
> in pstutter p q == toP [1,2,2,3,1,1,2,3,3]

> pstutter (toP [1,2,3]) (toP [4,5,6]) == toP [4,5,5,6,6,6]
> pstutter 2 (toP [4,5,6]) == toP [4,4,5,5,6,6]

Stutter scale degree and duration with the same random sequence.

> > Pbind(\n,Pwhite(3,10,inf),
> >       \degree,Pstutter(Pkey(\n),Pwhite(-4,11,inf)),
> >       \dur,Pstutter(Pkey(\n),Pwhite(0.05,0.4,inf)),
> >       \legato,0.3).play

> let {n = pwhite 'α' 3 10 inf
>     ;p = [(K_degree,pstutter n (pwhitei 'β' (-4) 11 inf))
>          ,(K_dur,pstutter n (pwhite 'γ' 0.05 0.4 inf))
>          ,(K_legato,0.3)]}
> in audition (pbind p)

-}
pstutter :: P Int -> P a -> P a
pstutter = liftP2_repeat P.stutter

-- | Pswitch.  Lifted 'P.switch'.
--
-- > let p = pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (toP [2,2,0,1])
-- > in p == toP [800,800,1,2,3,1,2,3,65,76]
pswitch :: [P a] -> P Int -> P a
pswitch l = liftP (P.switch (map unP l))

-- | Pswitch1.  Lifted /implicitly repeating/ 'P.switch1'.
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

-- | Ptuple.  'pseq' of 'ptranspose_st_repeat'.
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

{- | Pwhite.  Lifted 'P.white'.

> pwhite 'α' 0 9 5 == toP [3,0,1,6,6]
> pwhite 'α' 0 9 5 - pwhite 'α' 0 9 5 == toP [0,0,0,0,0]

The pattern below is alternately lower and higher noise.

> let {l = pseq [0.0,9.0] inf
>     ;h = pseq [1.0,12.0] inf}
> in audition (pbind [(K_freq,pwhite' 'α' l h * 20 + 800)
>                    ,(K_dur,0.25)])

-}
pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite = toP .::: P.white

{- | Pwrand.  Lifted 'P.wrand'.

> let w = C.normalizeSum [12,6,3]
> in pwrand 'α' [1,2,3] w 6 == toP [2,1,2,3,3,2]

> > r = Pwrand.new([1,2,Pseq([3,4],1)],[1,3,5].normalizeSum,6);
> > p = Pseed(Pn(100,1),r);
> > p.asStream.all == [2,3,4,1,3,4,3,4,2]

> let w = C.normalizeSum [1,3,5]
> in pwrand 'ζ' [1,2,pseq [3,4] 1] w 6 == toP [3,4,2,2,3,4,1,3,4]

> > Pbind(\degree,Pwrand((0..7),[4,1,3,1,3,2,1].normalizeSum,inf),
> >       \dur,0.25).play;

> let {w = C.normalizeSum [4,1,3,1,3,2,1]
>     ;d = pwrand 'α' (C.series 7 0 1) w inf}
> in audition (pbind [(K_degree,d),(K_dur,0.25)])

-}
pwrand :: (Enum e) => e -> [P a] -> [Double] -> Int -> P a
pwrand e a w = toP . P.wrand e (map unP a) w

-- | Pwrap.  Type specialised 'P.fwrap', see also 'pfold'.
--
-- > > p = Pwrap(Pgeom(200,1.25,9),200,1000.0);
-- > > r = p.asStream.all.collect({|n| n.round});
-- > > r == [200,250,313,391,488,610,763,954,392];
--
-- > let p = fmap roundE (pwrap (pgeom 200 1.25 9) 200 1000)
-- > in p == toP [200,250,312,391,488,610,763,954,391]
pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap = P.fwrap

-- | Pxrand.  Lifted 'P.xrand'.
--
-- > let p = pxrand 'α' [1,toP [2,3],toP [4,5,6]] 9
-- > in p == toP [4,5,6,2,3,4,5,6,1]
--
-- > > Pbind(\note,Pxrand([0,1,5,7],inf),\dur,0.25).play
--
-- > audition (pbind [(K_note,pxrand 'α' [0,1,5,7] inf),(K_dur,0.25)])
pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = toP (P.xrand e (map unP a) n)

-- * Variant SC3 Patterns

-- | Lifted /implicitly repeating/ 'P.pbrown''.
--
-- > pbrown' 'α' 1 700 (pseq [1,20] inf) 4 == toP [415,419,420,428]
pbrown' :: (Enum e,Random n,Num n,Ord n) =>
           e -> P n -> P n -> P n -> Int -> P n
pbrown' e l r s n =
    let f = liftP3_repeat (P.brown' e)
    in ptake n (f l r s)

-- | Un-joined variant of 'prand'.
--
-- > let p = prand' 'α' [1,toP [2,3],toP [4,5,6]] 5
-- > in p == toP [toP [4,5,6],toP [4,5,6],toP [2,3],toP [4,5,6],1]
prand' :: Enum e => e -> [P a] -> Int -> P (P a)
prand' e a n = toP (P.rand' e a n)

-- | Underlying pattern for 'prorate'.
--
-- > prorate' (Left 0.6) 0.5
prorate' :: Num a => Either a [a] -> a -> P a
prorate' p =
    case p of
      Left p' -> toP . P.rorate_n' p'
      Right p' -> toP . P.rorate_l' p'

{-|
Variant of `pseq` that retrieves only one value from each pattern
on each list traversal.  Compare to `pswitch1`.

> pseq [pseq [1,2] 1,pseq [3,4] 1] 2 == toP [1,2,3,4,1,2,3,4]
> pseq1 [pseq [1,2] 1,pseq [3,4] 1] 2 == toP [1,3,2,4]
> pseq1 [pseq [1,2] inf,pseq [3,4] inf] 3 == toP [1,3,2,4,1,3]

> let {p = prand' 'α' [pempty,toP [24,31,36,43,48,55]] inf
>     ;q = pflop [60,prand 'β' [63,65] inf
>                ,67,prand 'γ' [70,72,74] inf]
>     ;r = psplitPlaces (pwhite 'δ' 3 9 inf)
>                       (toP [74,75,77,79,81])
>     ;n = pjoin (pseq1 [p,q,r] inf)}
> in audition (pbind [(K_midinote,n),(K_dur,0.13)])

-}
pseq1 :: [P a] -> Int -> P a
pseq1 a i = join (ptake i (pflop a))

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

{-|

A variant of 'pseq' that passes a new seed at each invocation,
see also 'pfuncn'.

> > pseqr (\e -> [pshuf e [1,2,3,4] 1]) 2 == toP [2,3,4,1,4,1,2,3]

> let {d = pseqr (\e -> [pshuf e [-7,-3,0,2,4,7] 4
>                       ,pseq [0,1,2,3,4,5,6,7] 1]) inf}
> in audition (pbind [(K_degree,d),(K_dur,0.15)])

> > Pbind(\dur,0.2,
> >       \midinote,Pseq([Pshuf(#[60,61,62,63,64,65,66,67],3)],inf)).play

> let {m = pseqr (\e -> [pshuf e [60,61,62,63,64,65,66,67] 3]) inf}
> in audition (pbind [(K_dur,0.2),(K_midinote,m)])

-}
pseqr :: (Int -> [P a]) -> Int -> P a
pseqr f n = mconcat (L.concatMap f [1 .. n])

-- | Variant of 'pser' that consumes sub-patterns one element per
-- iteration.
--
-- > pser1 [1,pser [10,20] 3,3] 9 == toP [1,10,3,1,20,3,1,10,3]
pser1 :: [P a] -> Int -> P a
pser1 a i = ptake i (join (pflop a))

-- | Lifted /implicitly repeating/ 'P.pwhite'.
--
-- > pwhite' 'α' 0 (pseq [9,19] 3) == toP [3,0,1,6,6,15]
pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2_repeat (P.white' e)

-- | Lifted 'P.whitei'.
--
-- > pwhitei 'α' 1 9 5 == toP [5,1,7,7,8]
--
-- > audition (pbind [(K_degree,pwhitei 'α' 0 8 inf),(K_dur,0.15)])
pwhitei :: (RealFracE n,Random n,Enum e) => e -> n -> n -> Int -> P n
pwhitei =  toP .::: P.whitei

-- * Non-SC3 Patterns

-- | Type specialised 'P.fbool'.
pbool :: (Ord a,Num a) => P a -> P Bool
pbool = P.fbool

-- | 'mconcat' of 'replicate'.
pconcatReplicate :: Int -> P a -> P a
pconcatReplicate = mconcat .: replicate

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

-- | Lifted 'I.rsd'.
--
-- > prsd (pstutter 2 (toP [1,2,3])) == toP [1,2,3]
-- > prsd (pseq [1,2,3] 2) == toP [1,2,3,1,2,3]
prsd :: (Eq a) => P a -> P a
prsd = liftP I.rsd

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

-- * Aliases

-- | Type specialised 'mappend', sequences two patterns,
-- ie. 'Data.List.++'.
--
-- > 1 <> mempty <> 2 == toP [1,2]
--
-- > let {p = prand 'α' [0,1] 3
-- >     ;q = prand 'β' [5,7] 3}
-- > in audition (pbind [(K_degree,pappend p q),(K_dur,0.15)])
pappend :: P a -> P a -> P a
pappend = mappend

-- | Type specialised 'mconcat' (or equivalently 'msum' or
-- 'Data.List.concat').
--
-- > mconcat [pseq [1,2] 1,pseq [3,4] 2] == toP [1,2,3,4,3,4]
-- > msum [pseq [1,2] 1,pseq [3,4] 2] == toP [1,2,3,4,3,4]
pconcat :: [P a] -> P a
pconcat = mconcat

-- | Type specialised `mempty`, ie. 'Data.List.[]'.
pempty :: P a
pempty = mempty

-- | Type specialised 'F.foldr'.
--
-- > > (Pser([1,2,3],5) + Pseq([0,10],3)).asStream.all == [1,12,3,11,2]
--
-- > let p = pser [1,2,3] 5 + pseq [0,10] 3
-- > in F.foldr (:) [] p == [1,12,3,11,2]
--
-- Indefinte patterns may be folded.
--
-- > take 3 (F.foldr (:) [] (prepeat 1)) == [1,1,1]
--
-- The `Foldable` module includes functions for 'F.product', 'F.sum',
-- 'F.any', 'F.elem' etc.
--
-- > F.product (toP [1,3,5]) == 15
-- > floor (F.sum (ptake 100 (pwhite 'α' 0.25 0.75 inf))) == 51
-- > F.any even (toP [1,3,5]) == False
-- > F.elem 5 (toP [1,3,5]) == True
pfoldr :: (a -> b -> b) -> b -> P a -> b
pfoldr = F.foldr

-- | Type specialised 'join'.
--
-- > join (replicate 2 [1,2]) == [1,2,1,2]
-- > join (preplicate 2 (toP [1,2])) == toP [1,2,1,2]
pjoin :: P (P a) -> P a
pjoin = join

-- | 'join' that pushes an outer 'undecided' inward.
--
-- > join (undecided (undecided 1)) == undecided 1
-- > join (undecided (return 1)) == return 1
-- > pjoin_repeat (undecided (return 1)) == pure 1 == _|_
pjoin_repeat :: P (P a) -> P a
pjoin_repeat p =
    case p of
      P (Left (P (Right l))) -> toP (cycle l)
      _ -> join p

-- | Type specialised 'fmap', ie. 'Data.List.map'.
pmap :: (a -> b) -> P a -> P b
pmap = fmap

-- | Type specialised '>>='.
--
-- > (return 1 >>= return . id) == [1]
-- > (undecided 1 >>= undecided . id) == undecided 1
--
-- > (pseq [1,2] 1 >>= \x ->
-- >   pseq [3,4,5] 1 >>= \y ->
-- >    return (x,y)) == toP [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
pmbind :: P a -> (a -> P b) -> P b
pmbind = (>>=)

-- | Type specialised 'pure'.
ppure :: a -> P a
ppure = pure

-- | Type specialised 'return'.
preturn :: a -> P a
preturn = return

-- | Type specialised 'T.traverse'.
--
-- > let {f i e = (i + e,e * 2)
-- >     ;(r,p) = T.mapAccumL f 0 (toP [1,3,5])}
-- > in (r,p) == (9,toP [2,6,10])
ptraverse :: Applicative f => (a -> f b) -> P a -> f (P b)
ptraverse = T.traverse

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
