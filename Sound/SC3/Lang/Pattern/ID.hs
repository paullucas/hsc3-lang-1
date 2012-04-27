{-# Language FlexibleInstances #-}
-- | @sclang@ pattern library functions.
-- See <http://slavepianos.org/rd/?t=hsc3-texts> for tutorial.
module Sound.SC3.Lang.Pattern.ID where

import Control.Applicative hiding ((<*))
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Sound.OpenSoundControl
import Sound.SC3
import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Control.Event as E
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P
import qualified Sound.SC3.Lang.Math as M
import Sound.SC3.Lang.Pattern.List
import qualified Sound.SC3.Lang.Random.Gen as R
import System.Random

-- * P type and instances

-- | Pattern continuation mode
data M = Stop
       | Continue
         deriving (Eq,Show)

-- | Pattern data type (opaque)
data P a = P {unP :: [a]
             ,stP :: M}
    deriving (Eq,Show)

-- | A variant of 'pappend' that preserves the continuation mode but
-- is strict in the right argument.
pappend' :: P a -> P a -> P a
pappend' (P xs _) (P ys st) = P (xs ++ ys) st

-- | 'Data.Monoid.mappend' variant to sequence two patterns.
--
-- Note that in order for 'Data.Monoid.mappend' to be productive in
-- 'Data.Monoid.mconcat' on an infinite list it cannot store the
-- right-hand stop/continue mode, see 'pappend''
--
-- > toP [1,2] `pappend` toP [2,3] == toP [1,2,2,3]
-- > ptake 3 (prepeat 3 `pappend` prepeat 4) == toP' [3,3,3]
-- > ptake 3 (pconcat (cycle [prepeat 3])) == toP' [3,3,3]
-- > pempty `pappend` pempty == pempty
pappend :: P a -> P a -> P a
pappend p q = fromList (unP p ++ unP q)

instance Monoid (P a) where
    mappend = pappend
    mempty = P [] Continue

-- | A '>>=' variant using the continuation maintaining 'pappend'' function.
(>>=*) ::P a -> (a -> P b) -> P b
m >>=* k = F.foldr (pappend' . k) mempty m

instance Monad P where
    m >>= k = F.foldr (mappend . k) mempty m
    return x = P [x] Continue

instance Functor P where
    fmap f (P xs st) = P (map f xs) st

instance F.Foldable P where
    foldr f i (P xs _) = L.foldr f i xs

instance Applicative P where
    pure x = P [x] Continue
    f <*> e = fmap (\(f',e') -> f' e') (pzip f e)

instance Traversable P where
    traverse f (P xs st) = pure P <*> traverse f xs <*> pure st

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

instance (OrdE a) => OrdE (P a) where
    (>*) = pzipWith (>*)
    (>=*) = pzipWith (>=*)
    (<*) = pzipWith (<*)
    (<=*) = pzipWith (<=*)

-- | Pseudo-/infinite/ value for use at repeat counts.
inf :: Int
inf = maxBound

-- | Constant /NaN/ (not a number) value for use as a rest indicator
-- at a frequency model input (not at a @rest@ key).
nan :: (Monad m,Floating a) => m a
nan = return (sqrt (-1))

-- * Extension

-- | Join a set of 'M' values, if any are 'Stop' then 'Stop' else
-- 'Continue'.
stP_join :: [M] -> M
stP_join m = if L.any (== Stop) m then Stop else Continue

-- | Extension of a set of patterns.  If any patterns are stopping,
-- the longest such pattern, else the longest of the continuing
-- patterns.
--
-- > pextension [toP [1,2],toP [3,4,5]] == [(),(),()]
-- > pextension [toP' [1,2],toP [3,4,5]] == [(),()]
pextension :: [P a] -> [()]
pextension x =
    let x' = filter ((== Stop) . stP) x
    in if null x'
       then C.extension (map F.toList x)
       else C.extension (map F.toList x')

-- | Extend a set of patterns following 'pextension' rule.
--
-- > pextend [toP [1,2],toP [3,4,5]] == [toP' [1,2,1],toP' [3,4,5]]
--
-- > pextend [toP' [1,2],toP [3,4,5]] == [toP' [1,2],toP' [3,4]]
pextend :: [P a] -> [P a]
pextend l =
    let f = pzipWith (\_ x -> x) (P (pextension l) Stop) . pcycle
    in map f l

-- | Variant of 'transpose'.
--
-- > ptranspose [toP [1,2],toP [3,4,5]] == toP [[1,3],[2,4],[5]]
ptranspose :: [P a] -> P [a]
ptranspose l =
    let d = L.transpose (map unP l)
        s = stP_join (map stP l)
    in P d s

-- | Variant of 'pflop'.
--
-- > pflop' [toP [1,2],toP [3,4,5]] == toP' [[1,3],[2,4],[1,5]]
pflop' :: [P a] -> P [a]
pflop' l =
    let l' = map pcycle l
    in pzipWith (\_ x -> x) (P (pextension l) Stop) (ptranspose l')

-- | Variant of 'ptranspose' transforming the input patterns by
-- 'pextension'.
--
-- > pflop [toP [1,2],toP [3,4,5]] == toP' (map toP [[1,3],[2,4],[1,5]])
pflop :: [P a] -> P (P a)
pflop = fmap fromList . pflop'

-- | Composition of 'pjoin' and 'pflop'.
pflopJoin :: [P a] -> P a
pflopJoin = pjoin . pflop

-- * P lifting

-- | Lift unary list function to 'P'.
liftP :: ([a] -> [b]) -> P a -> P b
liftP f (P xs st) = P (f xs) st

-- | Lift binary list function to 'P'.
liftP2 :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2 f p q =
    let P l st = pzip p q
        (a,b) = unzip l
    in P (f a b) st

-- | Lift ternary list function to 'P'.
liftP3 :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3 f p q r =
    let P l st = pzip3 p q r
        (a,b,c) = unzip3 l
    in P (f a b c) st

-- | Lift quaternary list function to 'P'.
liftP4 :: ([a] -> [b] -> [c] -> [d] -> [e]) -> P a -> P b -> P c -> P d -> P e
liftP4 f p q r s =
    let P l st = pzip4 p q r s
        (a,b,c,d) = L.unzip4 l
    in P (f a b c d) st

-- * P functions

-- | Variant of 'null'.
pnull :: P a -> Bool
pnull = null . F.toList

-- | Select 'M' according to repeat count, see 'inf'.
stp :: Int -> M
stp n = if n == inf then Continue else Stop

-- | Set pattern mode to 'Stop'.
stopping :: P a -> P a
stopping (P xs _) = P xs Stop

-- | Set pattern mode according to repeat count, see 'inf'.
stoppingN :: Int -> P a -> P a
stoppingN n (P xs _) = P xs (stp n)

-- | Set pattern mode to 'Continue'.
continuing :: P a -> P a
continuing (P xs _) = P xs Continue

-- | The basic list to pattern function.  The pattern is continuing.
--
-- > continuing (pseq [1,2,3] 1) == toP [1,2,3]
fromList :: [a] -> P a
fromList xs = P xs Continue

-- | Alias for 'fromList'.
toP :: [a] -> P a
toP = fromList

-- | A variant from 'fromList' to make stopping patterns.
--
-- > pseq [1,2,3] 1 == toP' [1,2,3]
fromList' :: [a] -> P a
fromList' xs = P xs Stop

-- | Alias for 'fromList''.
toP' :: [a] -> P a
toP' = fromList'

-- | Pattern variant of 'repeat'. See also 'pure' and 'pcycle'.
--
-- > ptake 5 (prepeat 3) == toP' [3,3,3,3,3]
-- > ptake 5 (Control.Applicative.pure 3) == toP' [3]
-- > take 5 (Control.Applicative.pure 3) == [3]
prepeat :: a -> P a
prepeat = fromList . repeat

-- | Pattern variant of 'zipWith'.  Note that 'zipWith' is truncating,
-- whereas the numerical instances are extending.
--
-- > zipWith (*) [1,2,3] [5,6] == [5,12]
-- > pzipWith (*) (toP [1,2,3]) (toP [5,6]) == toP [5,12,15]
-- > toP [1,2,3] * toP [5,6] == toP [5,12,15]
--
-- Note that the list instance of applicative is combinatorial
-- (ie. Monadic).
--
-- > (pure (*) <*> [1,2,3] <*> [5,6]) == [5,6,10,12,15,18]
-- > (pure (*) <*> toP [1,2] <*> toP [5]) == toP [5,10]
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p q =
    let u = fmap (const ())
        x = pextension [u p,u q]
        c = cycle . unP
        l = zipWith3 (\_ i j -> f i j) x (c p) (c q)
    in P l (stP_join [stP p,stP q])

-- | Pattern variant of 'zipWith3'.
pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f p q r =
    let u = fmap (const ())
        x = pextension [u p,u q,u r]
        c = cycle . unP
        z = L.zipWith4 (\_ i j k -> f i j k) x (c p) (c q) (c r)
    in P z (stP_join [stP p,stP q,stP r])

-- | Pattern variant of 'zipWith4'.
pzipWith4 :: (a -> b -> c -> d -> e) -> P a -> P b -> P c -> P d -> P e
pzipWith4 f p q r s =
    let u = fmap (const ())
        x = pextension [u p,u q,u r,u s]
        c = cycle . unP
        z = L.zipWith5 (\_ i j k l -> f i j k l) x (c p) (c q) (c r) (c s)
    in P z (stP_join [stP p,stP q,stP r,stP s])

-- | Pattern variant of 'zip'.
--
-- > ptake 2 (pzip (prepeat 3) (prepeat 4)) == toP' [(3,4),(3,4)]
--
-- Note that haskell 'zip' is truncating wheras 'pzip' is extending.
--
-- > zip [1 .. 2] [0] == [(1,0)]
-- > pzip (toP [1..2]) (toP [0]) == toP [(1,0),(2,0)]
pzip :: P a -> P b -> P (a,b)
pzip = pzipWith (,)

-- | Pattern variant of 'zip3'.
pzip3 :: P a -> P b -> P c -> P (a,b,c)
pzip3 = pzipWith3 (,,)

-- | Tupling variant of 'pzipWith4'.
pzip4 :: P a -> P b -> P c -> P d -> P (a,b,c,d)
pzip4 = pzipWith4 (,,,)

-- | Pattern variant on 'unzip'.
punzip :: P (a,b) -> (P a,P b)
punzip (P p st) = let (i,j) = unzip p in (P i st,P j st)

-- * SC3 patterns

-- | Add a value to an existing key, or set the key if it doesn't exist.
--
-- > Padd(\freq,801,Pbind(\freq,100)).asStream.next(())
-- > padd "freq" 801 (pbind [("freq",100)]) == pbind [("freq",901)]
padd :: E.Key -> P E.Value -> P E.Event -> P E.Event
padd k = pzipWith (\i j -> E.edit_v k 0 (+ i) j)

-- | A primitive form of the SC3 'pbind' pattern, with explicit type
-- and identifier inputs.
pbind' :: [E.Type] -> [Maybe Int] -> [Maybe I.Instrument] -> [(E.Key,P E.Value)] -> P E.Event
pbind' ty is ss xs =
    let xs' =  pflop' (fmap (\(k,v) -> pzip (return k) v) xs)
        p = fromList
    in pure E.from_list <*> p ty <*> p is <*> p ss <*> xs'

-- | SC3 pattern to assign keys to a set of value patterns making an
-- 'E.Event' pattern. A finite binding stops the 'E.Event' pattern.
--
-- > Pbind(\x,Pseq([1,2,3]),
-- >       \y,Prand([100,300,200],inf)).asStream.nextN(3,())
--
-- > pkey "x" (pbind [("x",prand 'α' [100,300,200] inf)
-- >                 ,("y",pseq [1,2,3] 1)]) == toP' [200,200,300]
pbind :: [(E.Key,P E.Value)] -> P E.Event
pbind =
    let ty = repeat "s_new"
        i = repeat Nothing
        s = repeat Nothing
    in pbind' ty i s

-- | A variant of 'pbrown' where the l, r and s inputs are patterns.
--
-- > pbrown' 'α' 1 700 (pseq [1,20] inf) 4 == toP' [415,419,420,428]
pbrown' :: (Enum e,Random n,Num n,Ord n) => e -> P n -> P n -> P n -> Int -> P n
pbrown' e l r s n = let f = liftP3 (brown' e) in ptake n (f l r s)

-- | SC3 pattern to generate psuedo-brownian motion.
--
-- > pbrown 'α' 0 9 1 5 == toP' [4,4,5,4,3]
pbrown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> Int -> P n
pbrown e l r s n = ptake n (fromList (brown e l r s))

-- | SC3 sample and hold pattern.  For true values in the control
-- pattern, step the value pattern, else hold the previous value.
--
-- > Pclutch(Pser([1,2,3,4,5],8),
-- >         Pseq([1,0,1,0,0,0,1,1],inf)).asStream.all
--
-- > let {c = pbool (pseq [1,0,1,0,0,1,1] 1)
-- >     ;r = toP' [1,1,2,2,2,3,4,5,5,1,1,1,2,3]}
-- > in pclutch (pser [1,2,3,4,5] 8) c == r
--
-- Note the initialization behavior, nothing is generated until the
-- first true value.
--
-- > let {p = pseq [1,2,3,4,5] 1
-- >     ;q = pbool (pseq [0,0,0,0,0,0,1,0,0,1,0,1] 1)}
-- > in pclutch p q
pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

-- | SC3 name for 'fmap', ie. patterns are functors.
--
-- > Pcollect({arg i;i * 3},Pseq(#[1,2,3],inf)).asStream.nextN(9)
-- > pcollect (* 3) (toP [1,2,3]) == toP [3,6,9]
--
-- > Pseq(#[1,2,3],3).collect({arg i;i * 3}).asStream.nextN(9)
-- > fmap (* 3) (toP [1,2,3]) == toP [3,6,9]
pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

-- | SC3 pattern to constrain the sum of a numerical pattern.  Is
-- equal to /p/ until the accumulated sum is within /t/ of /n/.  At
-- that point, the difference between the specified sum and the
-- accumulated sum concludes the pattern.
--
-- > Pconst(10,Prand([1,2,0.5,0.1],inf),0.001).asStream.nextN(15,())
--
-- > let p = pconst 10 (prand 'α' [1,2,0.5,0.1] inf) 0.001
-- > in (p,Data.Foldable.sum p)
pconst :: (Ord a,Num a) => a -> P a -> a -> P a
pconst n p t =
    let f _ [] = []
        f j (i:is) = if i + j < n - t
                     then i : f (j + i) is
                     else [n - j]
    in stopping (fromList (f 0 (unP p)))

-- | SC3 pattern to derive notes from an index into a scale.
pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 (\i j k -> P.degree_to_key j k i)

-- | SC3 pattern to calculate adjacent element difference.
--
-- > pdiff (toP [0,2,3,5,6,8,9]) == toP [-2,-1,-2,-1,-2,-1,7]
pdiff :: Num n => P n -> P n
pdiff p = p - ptail p

-- | SC3 pattern to partition a value into /n/ equal subdivisions.
-- Subdivides each duration by each stutter and yields that value
-- stutter times.  A stutter of @0@ will skip the duration value, a
-- stutter of @1@ yields the duration value unaffected.
--
-- > s = Pseq(#[1,1,1,1,1,2,2,2,2,2,0,1,3,4,0],inf);
-- > d = Pseq(#[0.5,1,2,0.25,0.25],inf);
-- > PdurStutter(s,d).asStream.nextN(24)
--
-- > let {s = pseq [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0] inf
-- >     ;d = pseq [0.5,1,2,0.25,0.25] inf}
-- > in ptake 24 (pdurStutter s d)
pdurStutter :: Fractional a => P Int -> P a -> P a
pdurStutter = liftP2 durStutter

-- | Edit 'E.Value' at 'E.Key' in each element of an 'E.Event' pattern.
pedit :: E.Key -> (E.Value -> E.Value) -> P E.Event -> P E.Event
pedit k f = fmap (E.edit k f)

-- | An SC3 pattern of random values that follow a exponential
-- distribution.
--
-- > Pexprand(0.0001,1,10).asStream.all
-- > pexprand 'α' 0.0001 1 10
pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (M.exprandrng l r) (pwhite e 0 1 n)

-- | SC3 pattern to take the first n elements of the pattern.  See
-- also 'ptake'.
--
-- > Pfinval(5,Pseq(#[1,2,3],inf)).asStream.nextN(5)
-- > pfinval 5 (pseq [1,2,3] inf) == toP' [1,2,3,1,2]
pfinval :: Int -> P a -> P a
pfinval = ptake

-- | SC3 pattern to fold values to lie within range (as opposed to
-- wrap and clip).  This is /not/ related to the 'Data.Foldable'
-- pattern instance.
--
-- > pfold (toP [10,11,12,-6,-7,-8]) (-7) 11 == toP [10,11,10,-6,-7,-6]
--
-- The underlying primitive is the 'fold_' function.
--
-- > let f n = fold_ n (-7) 11
-- > in map f [10,11,12,-6,-7,-8] == [10,11,10,-6,-7,-6]
pfold :: (RealFrac n) => P n -> n -> n -> P n
pfold p i j = fmap (\n -> fold_ n i j) p

-- | Underlying form of haskell 'pfuncn' pattern.
pfuncn' :: (RandomGen g) => g -> (g -> (n,g)) -> Int -> P n
pfuncn' g_ f n =
  let go [] _ = []
      go (h:hs) g = let (r,g') = h g in r : go hs g'
  in P (go (replicate n f) g_) (stp n)

-- | A variant of the SC3 pattern that evaluates a closure at each
-- step.  The haskell variant function has a 'StdGen' form.
pfuncn :: (Enum e) => e -> (StdGen -> (n,StdGen)) -> Int -> P n
pfuncn e = pfuncn' (mkStdGen (fromEnum e))

-- | SC3 geometric series pattern.
--
-- > Pgeom(3,6,5).asStream.nextN(5)
-- > pgeom 3 6 5 == toP' [3,18,108,648,3888]
-- > pgeom 1 2 10 == toP' [1,2,4,8,16,32,64,128,256,512]
--
-- Real numbers work as well.
--
-- > pgeom 1.0 1.1 6
pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = P (C.geom n i s) Stop

-- | SC3 pattern-based conditional expression.
--
-- > var a = Pfunc({0.3.coin});
-- > var b = Pwhite(0,9,in);
-- > var c = Pwhite(10,19,inf);
-- > Pif(a,b,c).asStream.nextN(9)
--
-- > let {a = fmap (< 0.3) (pwhite 'α' 0.0 1.0 inf)
-- >     ;b = pwhite 'β' 0 9 inf
-- >     ;c = pwhite 'γ' 10 19 inf}
-- > in ptake 9 (pif a b c) == toP' [11,3,6,11,11,15,17,4,7]
pif :: P Bool -> P a -> P a -> P a
pif = liftP3 ifExtending

-- | Pattern to assign 'I.Instrument's to 'E.Event's.  An
-- 'I.Instrument' is either a 'Synthdef' or a 'String'.  In the
-- 'Synthdef' case the instrument is asynchronously sent to the server
-- before processing the event, which has timing implications.  In
-- general the instrument pattern ought to have a 'Synthdef' for the
-- first occurence of the instrument, and a 'String' for subsequent
-- occurences.
pinstr :: P I.Instrument -> P E.Event -> P E.Event
pinstr = pzipWith (\i e -> e {E.e_instrument = Just i})

-- | Variant of 'pinstr' which lifts the 'String' pattern to an
-- 'I.Instrument' pattern.
pinstr_s :: P (String,Bool) -> P E.Event -> P E.Event
pinstr_s p = pinstr (fmap (uncurry I.InstrumentName) p)

-- | Variant of 'pinstr' which lifts the 'Synthdef' pattern to an
-- 'I.Instrument' pattern.
pinstr_d :: P (Synthdef,Bool) -> P E.Event -> P E.Event
pinstr_d p = pinstr (fmap (uncurry I.InstrumentDef) p)

-- | Pattern to extract 'E.Value's at 'E.Key' from an 'E.Event'
-- pattern.
--
-- > pkey_m "freq" (pbind [("freq",440)]) == toP' [Just 440]
pkey_m :: E.Key -> P E.Event -> P (Maybe E.Value)
pkey_m k = fmap (E.lookup_m k)

-- | SC3 pattern to read 'E.Value' of 'E.Key' at 'E.Event' pattern.
-- Note however that in haskell is usually more appropriate to name
-- the pattern using /let/.
--
-- > pkey "freq" (pbind [("freq",440)]) == toP' [440]
-- > pkey "amp" (pbind [("amp",toP [0,1])]) == toP' [0,1]
pkey :: E.Key -> P E.Event -> P E.Value
pkey k = fmap (fromJust . E.lookup_m k)

-- | SC3 interlaced embedding of subarrays.
--
-- > Place([0,[1,2],[3,4,5]],3).asStream.all
-- > place [[0],[1,2],[3,4,5]] 3 == toP' [0,1,3,0,2,4,0,1,5]
--
-- > Place(#[1,[2,5],[3,6]],2).asStream.nextN(6)
-- > place [[1],[2,5],[3,6]] 2 == toP' [1,2,3,1,5,6]
-- > place [[1],[2,5],[3,6..]] 4 == toP' [1,2,3,1,5,6,1,2,9,1,5,12]
place :: [[a]] -> Int -> P a
place a n =
    let i = length a
        f = if n == inf then id else take (n * i)
    in stoppingN n (fromList (f (L.concat (C.flop a))))

-- | SC3 pattern that is a variant of 'pbind' for controlling
-- monophonic (persistent) synthesiser nodes.
pmono :: I.Instrument -> Int -> [(E.Key,P E.Value)] -> P E.Event
pmono i k =
    let i' = case i of
               I.InstrumentDef d sr ->
                   let nm = synthdefName d
                   in i : repeat (I.InstrumentName nm sr)
               I.InstrumentName _ _ -> repeat i
        ty = "s_new" : repeat "n_set"
    in pbind' ty (repeat (Just k)) (map Just i')

-- | Variant of 'pmono' that lifts 'Synthdef' to 'I.Instrument'.
pmono_d :: Synthdef -> Int -> [(E.Key,P E.Value)] -> P E.Event
pmono_d = pmono . flip I.InstrumentDef False

-- | Variant of 'pmono' that lifts 'String' to 'I.Instrument'.
pmono_s :: String -> Int -> [(E.Key,P E.Value)] -> P E.Event
pmono_s = pmono . flip I.InstrumentName False

-- | Idiom to scale 'E.Value' at 'E.Key' in an 'E.Event' pattern.
pmul :: E.Key -> P E.Value -> P E.Event -> P E.Event
pmul k = pzipWith (\i j -> E.edit_v k 1 (* i) j)

-- | Variant that does not insert key.
pmul' :: E.Key -> P E.Value -> P E.Event -> P E.Event
pmul' k = pzipWith (\i j -> E.edit k (* i) j)

-- | SC3 pattern to lace input patterns.  Note that the current
-- implementation stops late, it cycles the second series one place.
--
-- > ppatlace [1,prand 'α' [2,3] inf] 5 == toP' [1,3,1,2,1,3,1,2,1,2]
ppatlace :: [P a] -> Int -> P a
ppatlace a n =
    let i = length a
        f = if n == inf then id else take (n * i)
    in stoppingN n (P (f (L.concat (C.flop (map unP a)))) Continue)

-- | SC3 pattern to repeats the enclosed pattern a number of times.
--
-- > pn 1 4 == toP' [1,1,1,1]
-- > pn (toP [1,2,3]) 3 == toP' [1,2,3,1,2,3,1,2,3]
--
-- This is related to `concat`.`replicate` in standard list processing.
--
-- > concat (replicate 4 [1]) == [1,1,1,1]
-- > concat (replicate 3 [1,2,3]) == [1,2,3,1,2,3,1,2,3]
--
-- There is a `pconcatReplicate` near-alias (reversed argument order).
--
-- > pconcatReplicate 4 1 == toP' [1,1,1,1]
-- > pconcatReplicate 3 (toP [1,2]) == toP' [1,2,1,2,1,2]
--
-- This is productive over infinite lists.
--
-- > concat (replicate inf [1])
-- > pconcat (replicate inf 1)
-- > pconcatReplicate inf 1
pn :: P a -> Int -> P a
pn = flip pconcatReplicate

-- | Pattern variant of 'C.normalizeSum'.
pnormalizeSum :: Fractional n => P n -> P n
pnormalizeSum = liftP C.normalizeSum

-- | Un-joined variant of 'prand'.
prand' :: Enum e => e -> [P a] -> Int -> P (P a)
prand' e a n = P (rand' e a n) (stp n)

-- | SC3 pattern to make n random selections from a list of patterns,
-- the resulting pattern is flattened (joined).
--
-- > Prand([1,Pseq([10,20,30]),2,3,4,5],6).asStream.all
-- > prand 'α' [1,toP [10,20],2,3,4,5] 4 == toP' [5,2,10,20,2]
prand :: Enum e => e -> [P a] -> Int -> P a
prand e a = pjoin' . prand' e a

-- | SC3 pattern to rejects values for which the predicate is true.  reject
-- f is equal to filter (not . f).
--
-- > preject (== 1) (pseq [1,2,3] 2) == toP' [2,3,2,3]
-- > pfilter (not . (== 1)) (pseq [1,2,3] 2) == toP' [2,3,2,3]
--
-- > Pwhite(0,255,20).reject({|x| x.odd}).asStream.all
-- > preject odd (pwhite 'α' 0 255 10) == toP [32,158,62,216,240,20]
--
-- > Pwhite(0,255,20).select({|x| x.odd}).asStream.all
-- > pselect odd (pwhite 'α' 0 255 10) == toP [241,187,119,127]
preject :: (a -> Bool) -> P a -> P a
preject f = liftP (filter (not . f))

-- | Underlying pattern for 'prorate'.
prorate' :: Num a => Either a [a] -> a -> P a
prorate' p =
    case p of
      Left p' -> fromList . rorate_n' p'
      Right p' -> fromList . rorate_l' p'

-- | SC3 sub-dividing pattern.
--
-- > Prorate(Pseq([0.35,0.5,0.8]),1).asStream.nextN(6)
-- > prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1
--
-- > Prorate(Pseq([0.35,0.5,0.8]),Prand([20,1],inf)).asStream.nextN(6)
-- > prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) (prand 'α' [20,1] 3)
--
-- > var l = [[1,2],[5,7],[4,8,9]]).collect(_.normalizeSum);
-- > Prorate(Pseq(l,1).asStream.nextN(8)
--
-- > let l = map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]]
-- > in prorate (toP l) 1
prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate p = join . pzipWith prorate' p

-- | See 'pfilter'.
--
-- > pselect (< 3) (pseq [1,2,3] 2) == toP' [1,2,1,2]
pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

-- | Variant of `pseq` that retrieves only one value from each pattern
-- on each list traversal.  Compare to `pswitch1`.
--
-- > pseq [pseq [1,2] 1,pseq [3,4] 1] 2 == toP' [1,2,3,4,1,2,3,4]
-- > pseq1 [pseq [1,2] 1,pseq [3,4] 1] 2 == toP' [1,3,2,4]
-- > pseq1 [pseq [1,2] inf,pseq [3,4] inf] 3 == toP' [1,3,2,4,1,3]
pseq1 :: [P a] -> Int -> P a
pseq1 a i = pjoin' (ptake i (pflop a))

-- | SC3 pattern to cycle over a list of patterns. The repeats pattern
-- gives the number of times to repeat the entire list.
--
-- > pseq [return 1,return 2,return 3] 2 == toP' [1,2,3,1,2,3]
-- > pseq [1,2,3] 2 == toP' [1,2,3,1,2,3]
-- > pseq [1,pn 2 2,3] 2 == toP' [1,2,2,3,1,2,2,3]
--
-- There is an 'inf' value for the repeats variable.
--
-- > ptake 3 (pdrop 1000000 (pseq [1,2,3] inf)) == toP' [2,3,1]
pseq :: [P a] -> Int -> P a
pseq a i = stoppingN i (pn (pconcat a) i)

-- | A variant of 'pseq' that passes a new seed at each invocation,
-- see also 'pfuncn'.
pseqr :: (Int -> [P a]) -> Int -> P a
pseqr f n = pconcat (L.concatMap f [1 .. n])

-- | A variant of 'pseq' to aid translating a common SC3 idiom where a
-- finite random pattern is included in a @Pseq@ list.  In the SC3
-- case, at each iteration a new computation is run.  This idiom does
-- not directly translate to the declarative haskell pattern library.
--
-- > Pseq([1,Prand([2,3],1)],5).asStream.all
-- > pseq [1,prand 'α' [2,3] 1] 5
--
-- Although the intended pattern can usually be expressed using an
-- alternate construction:
--
-- > Pseq([1,Prand([2,3],1)],5).asStream.all
-- > ppatlace [1,prand 'α' [2,3] inf] 5 == toP' [1,3,1,2,1,3,1,2,1,2]
--
-- the 'pseqn' variant handles many common cases.
--
-- > Pseq([Pn(8,2),Pwhite(9,16,1)],5).asStream.all
-- > pseqn [2,1] [8,pwhite 'α' 9 16 inf] 5
pseqn :: [Int] -> [P a] -> Int -> P a
pseqn n q =
    let go _ 0 = pempty
        go p c = let (i,j) = unzip (zipWith psplitAt n p)
                 in pconcat i `pappend` go j (c - 1)
    in go (map pcycle q)

-- | Variant of 'pser' that consumes sub-patterns one element per
-- iteration.
--
-- > pser1 [1,pser [10,20] 3,3] 9 == toP' [1,10,3,1,20,3,1,10,3]
pser1 :: [P a] -> Int -> P a
pser1 a i = ptake i (pflopJoin a)

-- | SC3 pattern that is like 'pseq', however the repeats variable
-- gives the number of elements in the sequence, not the number of
-- cycles of the pattern.
--
-- > pser [1,2,3] 5 == toP' [1,2,3,1,2]
-- > pser [1,pser [10,20] 3,3] 9 == toP' [1,10,20,10,3,1,10,20,10]
-- > pser [1,2,3] 5 * 3 == toP' [3,6,9,3,6]
pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

-- | SC3 arithmetric series pattern, see also 'pgeom'.
--
-- > pseries 0 2 10 == toP' [0,2,4,6,8,10,12,14,16,18]
-- > pseries 9 (-1) 10 == toP' [9,8 .. 0]
-- > pseries 1.0 0.2 3 == toP' [1.0,1.2,1.4]
pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (C.series n i s) (stp n)

-- | SC3 pattern to return @n@ repetitions of a shuffled sequence.
--
-- > Pshuf([1,2,3,4],2).asStream.all
-- > pshuf 'α' [1,2,3,4] 2 == toP' [2,4,3,1,2,4,3,1]
pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a =
    let (a',_) = R.scramble a (mkStdGen (fromEnum e))
    in pn (P a' Continue)


-- | SC3 pattern to slide over a list of values.
--
-- > Pslide([1,2,3,4],inf,3,1,0).asStream.all
-- > pslide [1,2,3,4] 4 3 1 0 True == toP' [1,2,3,2,3,4,3,4,1,4,1,2]
-- > pslide [1,2,3,4,5] 3 3 (-1) 0 True == toP' [1,2,3,5,1,2,4,5,1]
pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide a n j s i = stoppingN n . fromList . slide a n j s i

-- | Pattern variant of 'splitAt'.
psplitAt :: Int -> P a -> (P a,P a)
psplitAt n (P p st) = let (i,j) = splitAt n p in (P i st,P j st)

-- | Pattern variant of 'S.splitPlaces'.
psplitPlaces' :: P Int -> P a -> P [a]
psplitPlaces' = liftP2 S.splitPlaces

-- | A variant of 'psplitPlaces'' that joins the output pattern.
psplitPlaces :: P Int -> P a -> P (P a)
psplitPlaces n = fmap fromList . psplitPlaces' n

-- | SC3 pattern to do time stretching.  It is equal to 'pmul' at
-- \"stretch\".
pstretch :: P E.Value -> P E.Event -> P E.Event
pstretch = pmul "stretch"

-- | SC3 pattern to repeat each element of a pattern _n_ times.
--
-- > pstutter 2 (toP [1,2,3]) == toP [1,1,2,2,3,3]
--
-- The count input may be a pattern.
--
-- > let {p = pseq [1,2] inf
-- >     ;q = pseq [1,2,3] 2}
-- > in pstutter p q == toP' [1,2,2,3,1,1,2,3,3]
--
-- > pstutter (toP [1,2,3]) (toP [4,5,6]) == toP [4,5,5,6,6,6]
pstutter :: P Int -> P a -> P a
pstutter = liftP2 stutterExtending

-- | SC3 pattern to select elements from a list of patterns by a
-- pattern of indices.
--
-- > switch l i = i >>= (l !!)
-- > pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (toP [2,2,0,1])
pswitch :: [P a] -> P Int -> P a
pswitch l = liftP (switch (map unP l))

-- | SC3 pattern that uses a pattern of indices to select which
-- pattern to retrieve the next value from.  Only one value is
-- selected from each pattern.  This is in comparison to 'pswitch',
-- which embeds the pattern in its entirety.
--
-- > Pswitch1([Pseq([1,2,3],inf),
-- >          Pseq([65,76],inf),
-- >          8],
-- >         Pseq([2,2,0,1],6)).asStream.all
--
-- > pswitch1 [pseq [1,2,3] inf,pseq [65,76] inf,8] (pseq [2,2,0,1] 6)
pswitch1 :: [P a] -> P Int -> P a
pswitch1 l = liftP (switch1 (map unP l))

-- | SC3 pattern to combine a list of streams to a stream of lists.
-- See also `pflop`.
--
-- > Ptuple([Pseries(7,-1,8),
-- >        Pseq([9,7,7,7,4,4,2,2],1),
-- >        Pseq([4,4,4,2,2,0,0,-3],1)],1).asStream.nextN(8)
--
-- > ptuple [pseries 7 (-1) 8
-- >        ,pseq [9,7,7,7,4,4,2,2] 1
-- >        ,pseq [4,4,4,2,2,0,0,-3] 1] 1
ptuple :: [P a] -> Int -> P [a]
ptuple p = pseq [pflop' p]

-- | A variant of 'pwhite' where the range inputs are patterns.
pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2 (white' e)

-- | SC3 pattern to generate a uniform linear distribution in given range.
--
-- > pwhite 'α' 0 9 5 == toP [3,0,1,6,6]
--
-- It is important to note that this structure is not actually
-- indeterminate, so that the below is zero.
--
-- > let p = pwhite 'α' 0.0 1.0 3 in p - p == toP [0,0,0]
pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = fromList . white e l r

-- | A variant of 'pwhite' that generates integral (rounded) values.
pwhitei :: (RealFrac n,Random n,Enum e) => e -> n -> n -> Int -> P n
pwhitei e l r = fmap roundf . pwhite e l r

-- | SC3 pattern to embed values randomly chosen from a list.  Returns
-- one item from the list at random for each repeat, the probability
-- for each item is determined by a list of weights which should sum
-- to 1.0.
--
-- > let w = C.normalizeSum [1,3,5]
-- > in pwrand 'α' [1,2,3] w 6 == toP [3,1,2,3,3,3]
--
-- Pwrand.new([1,2,Pseq([3,4],1)],[1,3,5].normalizeSum,6).asStream.nextN(6)
--
-- > let w = C.normalizeSum [1,3,5]
-- > in pwrand 'α' [1,2,pseq [3,4] 1] w 6 == toP [3,4,1,2,3,4]
pwrand :: Enum e => e -> [P a] -> [Double] -> Int -> P a
pwrand e a w n = P (wrand e (map unP a) w n) Continue

-- | SC3 pattern to constrain the range of output values by wrapping.
-- See also 'pfold'.
--
-- > Pn(Pwrap(Pgeom(200,1.07,26),200,1000.0),inf).asStream.nextN(26)
-- > pwrap (pgeom 200 1.07 26) 200 1000
pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap xs l r = fmap (genericWrap l r) xs

-- | SC3 pattern that is like 'prand' but filters successive duplicates.
--
-- > pxrand 'α' [1,toP [2,3],toP [4,5,6]] 15
pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = P (xrand e (map unP a) n) Continue

-- * Monoid aliases

-- | 'pconcat' is 'Data.Monoid.mconcat'.  See also 'pjoin'.
--
-- > take 3 (concat (replicate maxBound [1,2])) == [1,2,1]
-- > ptake 3 (pconcat (cycle [toP [1,2]])) == toP' [1,2,1]
-- > ptake 3 (pconcat [pseq [1,2] 1,pseq [3,4] 1]) == toP' [1,2,3]
pconcat :: [P a] -> P a
pconcat = mconcat

-- | Pattern variant for `Data.Monoid.mempty`, ie. the empty pattern.
--
-- > pempty `pappend` pempty == pempty
-- > pempty `pappend` 1 == 1 `pappend` pempty
pempty :: P a
pempty = mempty

-- * Monad aliases

-- | `Control.Monad.join` pattern variant.  See also `pconcat`.
--
-- > take 3 (Control.Monad.join (replicate maxBound [1,2])) == [1,2,1]
-- > ptake 3 (pjoin (preplicate inf (toP [1,2]))) == toP' [1,2,1]
pjoin :: P (P a) -> P a
pjoin = join

-- | Variant that maintains the continuing mode of the outer structure.
pjoin' :: P (P a) -> P a
pjoin' x = (join x) {stP = stP x}

-- * Data.List functions

-- | Pattern variant of ':'.
--
-- > pcons 'α' (pn (return 'β') 2) == fromList' "αββ"
pcons :: a -> P a -> P a
pcons i (P j st) = P (i:j) st

-- | Pattern variant of `cycle`.
--
-- > ptake 5 (pcycle (toP [1,2,3])) == toP' [1,2,3,1,2]
-- > ptake 5 (pseq [1,2,3] inf) == toP' [1,2,3,1,2]
pcycle :: P a -> P a
pcycle = continuing . liftP cycle

-- | Pattern variant of `drop`.
--
-- > Pseries(1,1,20).drop(5).asStream.nextN(15)
--
-- > pdrop 5 (pseries 1 1 10) == toP' [6,7,8,9,10]
-- > pdrop 1 pempty == pempty
pdrop :: Int -> P a -> P a
pdrop n = liftP (drop n)

-- | Pattern variant of `filter`.  Allows values for which the
-- predicate is true.  Aliased to `pselect`.  See also `preject`.
--
-- > pfilter (< 3) (pseq [1,2,3] 2) == toP' [1,2,1,2]
pfilter :: (a -> Bool) -> P a -> P a
pfilter f = liftP (filter f)

-- | Pattern variant of `replicate`.
--
-- > preplicate 4 1 == toP [1,1,1,1]
--
-- Compare to `pn`:
--
-- > pn 1 4 == toP' [1,1,1,1]
-- > pn (toP [1,2]) 3 == toP' [1,2,1,2,1,2]
-- > preplicate 4 (toP [1,2]) :: P (P Int)
preplicate :: Int -> a -> P a
preplicate n = fromList . replicate n

-- | Pattern variant of `scanl`.  `scanl` is similar to `foldl`, but
-- returns a list of successive reduced values from the left.
--
-- > Data.Foldable.foldl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == 43
-- > pscanl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == toP' [4,9,20,43]
pscanl :: (a -> b -> a) -> a -> P b -> P a
pscanl f i = liftP (L.scanl f i)

-- | Variant of 'drop', note that 'tail' is partial
--
-- > ptail (toP [1,2]) == toP [2]
-- > ptail pempty == pempty
ptail :: P a -> P a
ptail = pdrop 1

-- | Pattern variant of 'take', see also 'pfinval'.
--
-- > ptake 5 (pseq [1,2,3] 2) == toP' [1,2,3,1,2]
-- > ptake 5 (toP [1,2,3]) == toP' [1,2,3]
-- > ptake 5 (pseq [1,2,3] inf) == toP' [1,2,3,1,2]
-- > ptake 5 (pwhite 'α' 0 5 inf) == toP' [5,2,1,2,0]
--
-- Note that `ptake` does not extend the input pattern, unlike `pser`.
--
-- > ptake 5 (toP [1,2,3]) == toP' [1,2,3]
-- > pser [1,2,3] 5 == toP' [1,2,3,1,2]
ptake :: Int -> P a -> P a
ptake n = stoppingN n . liftP (take n)

-- * Non-SC3 patterns

-- | Transforms a numerical pattern into a boolean pattern where
-- values greater than zero are 'True' and zero and negative values
-- 'False'.
--
-- > pbool (toP [2,1,0,-1]) == toP [True,True,False,False]
pbool :: (Ord a,Num a) => P a -> P Bool
pbool = fmap (> 0)

-- | 'pconcat' '.' 'replicate', stopping after /n/ elements.
pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = stoppingN i . pconcat . replicate i

-- | Count the number of `False` values following each `True` value.
--
-- > pcountpost (pbool (pseq [1,0,1,0,0,0,1,1] 1)) == toP' [1,3,0,0]
pcountpost :: P Bool -> P Int
pcountpost = liftP countpost

-- | Count the number of `False` values preceding each `True` value.
--
-- > pcountpre (pbool (pseq [0,0,1,0,0,0,1,1] 1)) == toP' [2,3,0]
pcountpre :: P Bool -> P Int
pcountpre = liftP countpre

-- | Interleave elements from two patterns.  If one pattern ends the
-- other pattern continues until it also ends.
--
-- > let {p = pseq [1,2,3] 2
-- >     ;q = pseq [4,5,6,7] 1}
-- > in pinterleave p q == toP' [1,4,2,5,3,6,1,7,2,4,3,5]
--
-- > ptake 5 (pinterleave (pcycle 1) (pcycle 2)) == toP' [1,2,1,2,1]
-- > ptake 10 (pinterleave (pwhite 'α' 1 9 inf) (pseries 10 1 5))
pinterleave :: P a -> P a -> P a
pinterleave = liftP2 interleave

-- | Pattern to remove successive duplicates.
--
-- > prsd (pstutter 2 (toP [1,2,3])) == toP [1,2,3]
-- > prsd (pseq [1,2,3] 2) == toP' [1,2,3,1,2,3]
prsd :: (Eq a) => P a -> P a
prsd = liftP rsd

-- | Pattern where the 'tr' pattern determines the rate at which
-- values are read from the `x` pattern.  For each sucessive true
-- value at 'tr' the output is a `Just e` of each succesive element at
-- x.  False values at 'tr' generate `Nothing` values.
--
-- > let {tr = pbool (toP [0,1,0,0,1,1])
-- >     ;r = [Nothing,Just 1,Nothing,Nothing,Just 2,Just 3]}
-- > in ptrigger tr (toP [1,2,3]) == fromList r
ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing `pappend` return (Just x)
    in pjoin (pzipWith f r q)

-- * Parallel patterns

-- | Merge two 'E.Event' patterns with indicated start 'E.Time's.
ptmerge :: (E.Time,P E.Event) -> (E.Time,P E.Event) -> P E.Event
ptmerge (pt,p) (qt,q) =
    fromList (E.merge (pt,F.toList p) (qt,F.toList q))

-- | Variant of 'ptmerge' with zero start times.
pmerge :: P E.Event -> P E.Event -> P E.Event
pmerge p q = ptmerge (0,p) (0,q)

-- | Merge a set of 'E.Event' patterns each with indicated start 'E.Time'.
ptpar :: [(E.Time,P E.Event)] -> P E.Event
ptpar l =
    case l of
      [] -> pempty
      [(_,p)] -> p
      (pt,p):(qt,q):r -> ptpar ((min pt qt,ptmerge (pt,p) (qt,q)) : r)

-- | Variant of 'ptpar' with zero start times.
ppar :: [P E.Event] -> P E.Event
ppar l = ptpar (zip (repeat 0) l)

-- * Pattern audition

-- | Send 'E.Event' to @scsynth@ at 'Transport'.
e_send :: Transport t => t -> E.Time -> Int -> E.Event -> IO ()
e_send fd t j e =
    case E.to_sc3_bundle t j e of
      Just (p,q) -> do case E.instrument_def e of
                         Just d -> async fd (d_recv d) >> return ()
                         Nothing -> return ()
                       send fd p
                       send fd q
      Nothing -> return ()

-- | Function to audition a sequence of 'E.Event's using the @scsynth@
-- instance at 'Transport' starting at indicated 'E.Time'.
e_tplay :: (Transport t) => t -> E.Time -> [Int] -> [E.Event] -> IO ()
e_tplay fd t j e =
    case (j,e) of
      (_,[]) -> return ()
      ([],_) -> error "e_tplay: no-id"
      (i:j',d:e') -> do let t' = t + E.fwd d
                        e_send fd t i d
                        pauseThreadUntil t'
                        e_tplay fd t' j' e'

-- | Variant of 'e_tplay' with current clock time from 'utcr' as start
-- time.  This function is used to implement the pattern instances of
-- 'Audible'.
e_play :: (Transport t) => t -> [Int] -> [E.Event] -> IO ()
e_play fd lj le = do
  st <- utcr
  e_tplay fd st lj le

instance Audible (P E.Event) where
    play fd = e_play fd [1000..] . unP

instance Audible (Synthdef,P E.Event) where
    play fd (s,p) = do
      let i_d = I.InstrumentDef s True
          i_nm = I.InstrumentName (synthdefName s) True
          i = pcons i_d (pn (return i_nm) inf)
      _ <- async fd (d_recv s)
      e_play fd [1000..] (unP (pinstr i p))

instance Audible (String,P E.Event) where
    play fd (s,p) =
        let i = I.InstrumentName s True
        in e_play fd [1000..] (unP (pinstr (return i) p))

