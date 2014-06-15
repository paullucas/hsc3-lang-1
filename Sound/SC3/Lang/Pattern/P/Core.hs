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
module Sound.SC3.Lang.Pattern.P.Core where

import Control.Applicative hiding ((<*)) {- base -}
import Control.Monad {- base -}
import Data.Bifunctor {- bifunctors -}
import qualified Data.Foldable as F {- base -}
import qualified Data.List as L {- base -}
import Data.Monoid {- base -}
import qualified Data.Traversable as T {- base -}

import Sound.SC3 (OrdE(..)) {- hsc3 -}

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
