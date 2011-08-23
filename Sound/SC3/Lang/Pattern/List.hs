{-# Language FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.List where

import Control.Applicative hiding ((<*))
import Control.Monad
import Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Collection.Event
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import qualified Sound.SC3.Lang.Math.Pitch as P
import qualified Sound.SC3.Lang.Math.SimpleNumber as N
import System.Random

-- * P type and instances

data M = Stop | Continue deriving (Eq,Show)
data P a = P {unP :: [a]
             ,stP :: M}
    deriving (Eq,Show)

-- | A variant that preserves the continuation mode but is strict in
--   it's right argument.
pappend' :: P a -> P a -> P a
pappend' (P xs _) (P ys st) = P (xs ++ ys) st

pappend :: P a -> P a -> P a
pappend p q = fromList (unP p ++ unP q)

-- in order for mappend to be productive in mconcat on an infinite
-- list it cannot store the right-hand stop/continue mode
instance Monoid (P a) where
    mappend = pappend
    mempty = P [] Continue

-- | A variant using the continuation maintaining pappend' function.
(>>=*) ::P a -> (a -> P b) -> P b
m >>=* k = F.foldr (pappend' . k) mempty m

instance Monad P where
    m >>= k = F.foldr (mappend . k) mempty m
    return x = P [x] Continue

instance Functor P where
    fmap f (P xs st) = P (map f xs) st

instance Foldable P where
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

inf :: Int
inf = maxBound

nan :: (Monad m,Floating a) => m a
nan = return (sqrt (-1))

-- * Extension

stP_join :: [M] -> M
stP_join m = if L.any (== Stop) m then Stop else Continue

pextension :: [P a] -> [()]
pextension x =
    let x' = filter ((== Stop) . stP) x
    in if null x'
       then C.extension (map toList x)
       else C.extension (map toList x')

pextend :: [P a] -> [P a]
pextend l =
    let f = pzipWith (\_ x -> x) (P (pextension l) Stop) . pcycle
    in map f l

ptranspose :: [P a] -> P [a]
ptranspose l =
    let d = L.transpose (map unP l)
        s = stP_join (map stP l)
    in P d s

pflop' :: [P a] -> P [a]
pflop' l =
    let l' = map pcycle l
    in pzipWith (\_ x -> x) (P (pextension l) Stop) (ptranspose l')

pflop :: [P a] -> P (P a)
pflop = fmap fromList . pflop'

pflopJoin :: [P a] -> P a
pflopJoin = join . pflop

-- * P lifting

liftP :: ([a] -> [b]) -> P a -> P b
liftP f (P xs st) = P (f xs) st

liftP2 :: ([a] -> [b] -> [c]) -> P a -> P b -> P c
liftP2 f p q =
    let P l st = pzip p q
        (a,b) = unzip l
    in P (f a b) st

liftP3 :: ([a] -> [b] -> [c] -> [d]) -> P a -> P b -> P c -> P d
liftP3 f p q r =
    let P l st = pzip3 p q r
        (a,b,c) = unzip3 l
    in P (f a b c) st

liftP4 :: ([a] -> [b] -> [c] -> [d] -> [e]) -> P a -> P b -> P c -> P d -> P e
liftP4 f p q r s =
    let P l st = pzip4 p q r s
        (a,b,c,d) = L.unzip4 l
    in P (f a b c d) st

-- * P functions

pnull :: P a -> Bool
pnull = null . toList

stp :: Int -> M
stp n = if n == inf then Continue else Stop

stopping :: P a -> P a
stopping (P xs _) = P xs Stop

stoppingN :: Int -> P a -> P a
stoppingN n (P xs _) = P xs (stp n)

continuing :: P a -> P a
continuing (P xs _) = P xs Continue

fromList :: [a] -> P a
fromList xs = P xs Continue

fromList' :: [a] -> P a
fromList' xs = P xs Stop

prepeat :: a -> P a
prepeat = fromList . repeat

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f p q =
    let u = fmap (const ())
        x = pextension [u p,u q]
        c = cycle . unP
        l = zipWith3 (\_ i j -> f i j) x (c p) (c q)
    in P l (stP_join [stP p,stP q])

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f p q r =
    let u = fmap (const ())
        x = pextension [u p,u q,u r]
        c = cycle . unP
        z = L.zipWith4 (\_ i j k -> f i j k) x (c p) (c q) (c r)
    in P z (stP_join [stP p,stP q,stP r])

pzipWith4 :: (a -> b -> c -> d -> e) -> P a -> P b -> P c -> P d -> P e
pzipWith4 f p q r s =
    let u = fmap (const ())
        x = pextension [u p,u q,u r,u s]
        c = cycle . unP
        z = L.zipWith5 (\_ i j k l -> f i j k l) x (c p) (c q) (c r) (c s)
    in P z (stP_join [stP p,stP q,stP r,stP s])

pzip :: P a -> P b -> P (a,b)
pzip = pzipWith (,)

pzip3 :: P a -> P b -> P c -> P (a,b,c)
pzip3 = pzipWith3 (,,)

pzip4 :: P a -> P b -> P c -> P d -> P (a,b,c,d)
pzip4 = pzipWith4 (,,,)

punzip :: P (a,b) -> (P a,P b)
punzip (P p st) = let (i,j) = unzip p in (P i st,P j st)

-- * SC3 patterns

padd :: Key -> P Value -> P Event -> P Event
padd k p = pzipWith (\i j -> e_edit_v k 0 (+ i) j) p

pbind' :: [Type] -> [Int] -> [Instrument] -> [(String,P Value)] -> P Event
pbind' ty is ss xs =
    let xs' =  pflop' (fmap (\(k,v) -> pzip (return k) v) xs)
        p = fromList
    in pure e_from_list <*> p ty <*> p is <*> p ss <*> xs'

pbind :: [(String,P Value)] -> P Event
pbind =
    let ty = repeat "s_new"
        i = repeat (-2)
        s = repeat (InstrumentName "default")
    in pbind' ty i s

brown_ :: (RandomGen g,Random n,Num n,Ord n) => (n,n,n) -> (n,g) -> (n,g)
brown_ (l,r,s) (n,g) =
    let (i,g') = randomR (-s,s) g
    in (fold' l r (n + i),g')

brown' :: (Enum e,Random n,Num n,Ord n) => e -> [n] -> [n] -> [n] -> [n]
brown' e l_ r_ s_ =
    let go _ [] = []
        go (n,g) ((l,r,s):z) = let (n',g') = brown_ (l,r,s) (n,g)
                               in n' : go (n',g') z
    in go (randomR (head l_,head r_) (mkStdGen (fromEnum e))) (zip3 l_ r_ s_)

pbrown' :: (Enum e,Random n,Num n,Ord n) => e -> P n -> P n -> P n -> Int -> P n
pbrown' e l r s n = let f = liftP3 (brown' e) in ptake n (f l r s)

brown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> [n]
brown e l r s = brown' e (repeat l) (repeat r) (repeat s)

pbrown :: (Enum e,Random n,Num n,Ord n) => e -> n -> n -> n -> Int -> P n
pbrown e l r s n = ptake n (fromList (brown e l r s))

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pconst :: (Ord a,Num a) => a -> P a -> a -> P a
pconst n p t =
    let f _ [] = []
        f j (i:is) = if i + j < n - t
                     then i : f (j + i) is
                     else [n - j]
    in stopping (fromList (f 0 (unP p)))

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 P.degree_to_key

pdiff :: Num n => P n -> P n
pdiff p = p - ptail p

durStutter :: Fractional a => [Int] -> [a] -> [a]
durStutter p =
    let f s d = case s of
                0 -> []
                1 -> [d]
                _ -> replicate s (d / fromIntegral s)
    in L.concat . C.zipWith_c f p

pdurStutter :: Fractional a => P Int -> P a -> P a
pdurStutter = liftP2 durStutter

pedit :: Key -> (Value -> Value) -> P Event -> P Event
pedit k f = fmap (e_edit k f)

pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (N.exprandrng l r) (pwhite e 0 1 n)

pfinval :: Int -> P a -> P a
pfinval = ptake

pfold :: (RealFrac n) => P n -> n -> n -> P n
pfold p i j = fmap (\n -> fold_ n i j) p

pfuncn' :: (RandomGen g) => g -> (g -> (n,g)) -> Int -> P n
pfuncn' g_ f n =
  let go [] _ = []
      go (h:hs) g = let (r,g') = h g in r : go hs g'
  in P (go (replicate n f) g_) (stp n)

pfuncn :: (Enum e) => e -> (StdGen -> (n,StdGen)) -> Int -> P n
pfuncn e f n = pfuncn' (mkStdGen (fromEnum e)) f n

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = P (C.geom n i s) Stop

ifF :: Bool -> a -> a -> a
ifF x y z = if x then y else z

ifF' :: (Bool,a,a) -> a
ifF' (x,y,z) = if x then y else z

ifTruncating :: [Bool] -> [a] -> [a] -> [a]
ifTruncating  a b c = map ifF' (zip3 a b c)

ifExtending :: [Bool] -> [a] -> [a] -> [a]
ifExtending a b c = map ifF' (C.zip3_c a b c)

pif :: P Bool -> P a -> P a -> P a
pif = liftP3 ifExtending

pinstr :: P Instrument -> P Event -> P Event
pinstr p = pzipWith (\i e -> e {e_instrument = i}) p

pinstr_s :: P String -> P Event -> P Event
pinstr_s p = pinstr (fmap InstrumentName p)

pinstr_d :: P Synthdef -> P Event -> P Event
pinstr_d p = pinstr (fmap InstrumentDef p)

place :: [[a]] -> Int -> P a
place a n =
    let i = length a
        f = if n == inf then id else take (n * i)
    in stoppingN n (fromList (f (L.concat (C.flop a))))

pmono_d :: Synthdef -> Int -> [(String,P Double)] -> P Event
pmono_d s i =
    let ss = InstrumentDef s : repeat (InstrumentName (synthdefName s))
        ty = "s_new_p" : repeat "n_set_p"
    in pbind' ty (repeat i) ss

pmono_s :: String -> Int -> [(String,P Double)] -> P Event
pmono_s s i =
    let ss = repeat (InstrumentName s)
        ty = "s_new_p" : repeat "n_set_p"
    in pbind' ty (repeat i) ss

pmul :: Key -> P Value -> P Event -> P Event
pmul k p = pzipWith (\i j -> e_edit_v k 1 (* i) j) p

-- | Variant that does not insert key.
pmul' :: Key -> P Value -> P Event -> P Event
pmul' k p = pzipWith (\i j -> e_edit k (* i) j) p

ppatlace :: [P a] -> Int -> P a
ppatlace a n =
    let i = length a
        f = if n == inf then id else take (n * i)
    in stoppingN n (P (f (L.concat (C.flop (map unP a)))) Continue)

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

pnormalizeSum :: Fractional n => P n -> P n
pnormalizeSum = liftP C.normalizeSum

rand' :: Enum e => e -> [a] -> Int -> [a]
rand' e a n =
    let k = length a - 1
        f m g = if m == 0
                then []
                else let (i,g') = randomR (0,k) g
                     in (a !! i) : f (m - 1) g'
    in f n (mkStdGen (fromEnum e))

prand' :: Enum e => e -> [P a] -> Int -> P (P a)
prand' e a n = P (rand' e a n) (stp n)

prand :: Enum e => e -> [P a] -> Int -> P a
prand e a = pjoin' . prand' e a

preject :: (a -> Bool) -> P a -> P a
preject f = liftP (filter (not . f))

rorate_n' :: Num a => a -> a -> [a]
rorate_n' p i = [i * p,i * (1 - p)]

rorate_n :: Num a => [a] -> [a] -> [a]
rorate_n p = L.concat . C.zipWith_c rorate_n' p

rorate_l' :: Num a => [a] -> a -> [a]
rorate_l' p i = map (* i) p

rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate_l p = L.concat . C.zipWith_c rorate_l' p

prorate' :: Num a => Either a [a] -> a -> P a
prorate' p =
    case p of
      Left p' -> fromList . rorate_n' p'
      Right p' -> fromList . rorate_l' p'

prorate :: Num a => P (Either a [a]) -> P a -> P a
prorate p = join . pzipWith prorate' p

pselect :: (a -> Bool) -> P a -> P a
pselect f = liftP (filter f)

pseq1 :: [P a] -> Int -> P a
pseq1 a i = pjoin' (ptake i (pflop a))

pseq :: [P a] -> Int -> P a
pseq a i = stoppingN i (pn (pconcat a) i)

pseqr :: (Int -> [P a]) -> Int -> P a
pseqr f n = pconcat (L.concatMap f [1 .. n])

pseqn :: [Int] -> [P a] -> Int -> P a
pseqn n q =
    let go _ 0 = pempty
        go p c = let (i,j) = unzip (zipWith psplitAt n p)
                 in pconcat i `pappend` go j (c - 1)
    in go (map pcycle q)

pser1 :: [P a] -> Int -> P a
pser1 a i = ptake i (pflopJoin a)

pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (C.series n i s) (stp n)

pshuf :: Enum e => e -> [a] -> Int -> P a
pshuf e a =
    let (a',_) = C.scramble' a (mkStdGen (fromEnum e))
    in pn (P a' Continue)

wrap' :: (Num a,Ord a) => (a,a) -> a -> a
wrap' (l,r) i =
    let d = r - l
        f = wrap' (l,r)
    in if i < l then f (i + d) else if i >= r then f (i - d) else i

segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (wrap' (0,k)) [l .. r]
    in map (a !!) i

slide :: [a] -> Int -> Int -> Int -> Int -> Bool -> [a]
slide a n j s i wr =
    let k = length a
        l = enumFromThen i (i + s)
        r = map (+ (j - 1)) l
    in if wr
       then L.concat (take n (map (segment a k) (zip l r)))
       else error "slide: non-wrap variant not implemented"

pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a
pslide a n j s i = stoppingN n . fromList . slide a n j s i

psplitAt :: Int -> P a -> (P a,P a)
psplitAt n (P p st) = let (i,j) = splitAt n p in (P i st,P j st)

psplitPlaces' :: P Int -> P a -> P [a]
psplitPlaces' = liftP2 S.splitPlaces

psplitPlaces :: P Int -> P a -> P (P a)
psplitPlaces n = fmap fromList . psplitPlaces' n

pstretch :: P Value -> P Event -> P Event
pstretch = pmul "stretch"

stutterTruncating :: [Int] -> [a] -> [a]
stutterTruncating ns = L.concat . zipWith replicate ns

stutterExtending :: [Int] -> [a] -> [a]
stutterExtending ns = L.concat . C.zipWith_c replicate ns

pstutter :: P Int -> P a -> P a
pstutter = liftP2 stutterExtending

switch :: [[a]] -> [Int] -> [a]
switch l i = i >>= (l !!)

pswitch :: [P a] -> P Int -> P a
pswitch l = liftP (switch (map unP l))

switch1 :: [[a]] -> [Int] -> [a]
switch1 ps =
    let go _ [] = []
        go m (i:is) = case M.lookup i m of
                        Nothing -> []
                        Just [] -> []
                        Just (x:xs) -> x : go (M.insert i xs m) is
    in go (M.fromList (zip [0..] (C.extendSequences ps)))

pswitch1 :: [P a] -> P Int -> P a
pswitch1 l = liftP (switch1 (map unP l))

ptuple :: [P a] -> Int -> P [a]
ptuple p = pseq [pflop' p]

white' :: (Enum e,Random n) => e -> [n] -> [n] -> [n]
white' e l r =
    let g = mkStdGen (fromEnum e)
        n = zip l r
        f a b = let (a',b') = randomR b a in (b',a')
    in snd (L.mapAccumL f g n)

pwhite' :: (Enum e,Random n) => e -> P n -> P n -> P n
pwhite' e = liftP2 (white' e)

white :: (Random n,Enum e) => e -> n -> n -> Int -> [n]
white e l r n = take n (randomRs (l,r) (mkStdGen (fromEnum e)))

pwhite :: (Random n,Enum e) => e -> n -> n -> Int -> P n
pwhite e l r = fromList . white e l r

pwhitei :: (RealFracE n,Random n,Enum e) => e -> n -> n -> Int -> P n
pwhitei e l r = fmap roundE . pwhite e l r

wrand' :: (Enum e) =>e -> [[a]] -> [Double] -> [a]
wrand' e a w =
    let f g = let (r,g') = C.wchoose' a w g
              in r ++ f g'
    in f (mkStdGen (fromEnum e))

wrand :: (Enum e) => e -> [[a]] -> [Double] -> Int -> [a]
wrand e a w n = take n (wrand' e a w)

pwrand :: Enum e => e -> [P a] -> [Double] -> Int -> P a
pwrand e a w n = P (wrand e (map unP a) w n) Continue

pwrap :: (Ord a,Num a) => P a -> a -> a -> P a
pwrap xs l r = fmap (wrap' (l,r)) xs

xrand' :: Enum e => e -> [[a]] -> [a]
xrand' e a =
    let k = length a - 1
        f j g = let (i,g') = randomR (0,k) g
                in if i == j then f j g' else (a !! i) ++ f i g'
    in f (-1) (mkStdGen (fromEnum e))

xrand :: Enum e => e -> [[a]] -> Int -> [a]
xrand e a n = take n (xrand' e a)

pxrand :: Enum e => e -> [P a] -> Int -> P a
pxrand e a n = P (xrand e (map unP a) n) Continue

-- * Monoid aliases

pconcat :: [P a] -> P a
pconcat = mconcat

pempty :: P a
pempty = mempty

-- * Monad aliases

pjoin :: P (P a) -> P a
pjoin = join

-- | Variant that maintains the continuing mode of the outer structure.
pjoin' :: P (P a) -> P a
pjoin' x = (join x) {stP = stP x}

-- * Data.List functions

pcons :: a -> P a -> P a
pcons i (P j st) = P (i:j) st

pcycle :: P a -> P a
pcycle = continuing . liftP cycle

pdrop :: Int -> P a -> P a
pdrop n = liftP (drop n)

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = liftP (filter f)

preplicate :: Int -> a -> P a
preplicate n = fromList . replicate n

pscanl :: (a -> b -> a) -> a -> P b -> P a
pscanl f i = liftP (L.scanl f i)

-- | Data.List.tail is partial
ptail :: P a -> P a
ptail = pdrop 1

ptake :: Int -> P a -> P a
ptake n = stoppingN n . liftP (take n)

-- * Non-SC3 patterns

pbool :: (Ord a,Num a) => P a -> P Bool
pbool = fmap (> 0)

pconcatReplicate :: Int -> P a -> P a
pconcatReplicate i = stoppingN i . pconcat . replicate i

countpost :: [Bool] -> [Int]
countpost =
    let f i p = if null p
                then [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if not x then f (i + 1) xs else r
    in tail . f 0

pcountpost :: P Bool -> P Int
pcountpost = liftP countpost

countpre :: [Bool] -> [Int]
countpre =
    let f i p = if null p
                then if i == 0 then [] else [i]
                else let (x:xs) = p
                         r = i : f 0 xs
                     in if x then r else f (i + 1) xs
    in f 0

pcountpre :: P Bool -> P Int
pcountpre = liftP countpre

interleave :: [a] -> [a] -> [a]
interleave p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      (x:xs,y:ys) -> x : y : interleave xs ys

pinterleave :: P a -> P a -> P a
pinterleave = liftP2 interleave

rsd :: (Eq a) => [a] -> [a]
rsd =
    let f (p,_) i = (Just i,if Just i == p then Nothing else Just i)
    in mapMaybe snd . scanl f (Nothing,Nothing)

prsd :: (Eq a) => P a -> P a
prsd = liftP rsd

trigger :: [Bool] -> [a] -> [Maybe a]
trigger p q =
    let r = countpre p
        f i x = replicate i Nothing ++ [Just x]
    in L.concat (C.zipWith_c f r q)

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger = liftP2 trigger

-- * Parallel patterns

f_merge :: Ord a => [(a,t)] -> [(a,t)] -> [(a,t)]
f_merge p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      ((t0,e0):r0,(t1,e1):r1) ->
            if t0 <= t1
            then (t0,e0) : f_merge r0 q
            else (t1,e1) : f_merge p r1

{-
f_merge (zip [0,2..10] ['a'..]) (zip [0,4..12] ['A'..])
-}

type T = Double

-- note that this uses e_fwd to calculate start times.
e_merge :: (T,[Event]) -> (T,[Event]) -> [(T,Event)]
e_merge (pt,p) (qt,q) =
    let p_st = map (+ pt) (0 : scanl1 (+) (map e_fwd p))
        q_st = map (+ qt) (0 : scanl1 (+) (map e_fwd q))
    in f_merge (zip p_st p) (zip q_st q)

add_fwd :: [(Double,Event)] -> [Event]
add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' -> e_set_fwd (t1 - t0) e0 : add_fwd ((t1,e1):e')
      _ -> map snd e

ptmerge :: (T,P Event) -> (T,P Event) -> P Event
ptmerge (pt,p) (qt,q) =
    fromList (add_fwd (e_merge (pt,toList p) (qt,toList q)))

pmerge :: P Event -> P Event -> P Event
pmerge p q = ptmerge (0,p) (0,q)

ptpar :: [(T,P Event)] -> P Event
ptpar l =
    case l of
      [] -> pempty
      [(_,p)] -> p
      (pt,p):(qt,q):r -> ptpar ((min pt qt,ptmerge (pt,p) (qt,q)) : r)

ppar :: [P Event] -> P Event
ppar l = ptpar (zip (repeat 0) l)

-- * Pattern audition

-- t = time, s = instrument name
-- rt = release time, pr = parameters
-- ty:_p suffix (p = persist) does not send gate
e_osc :: Double -> Int -> Event -> Maybe (OSC,OSC)
e_osc t j e =
    let s = e_instrument_name e
        rt = e_sustain e
        f = e_freq e
        a = e_amp e
        pr = ("freq",f) : ("amp",a) : e_arg e
        i = if e_id e < -1 then j else e_id e
    in if isNaN f
       then Nothing
       else let m_on = case e_type e of
                         "s_new" -> [s_new s i AddToTail 1 pr]
                         "s_new_p" -> [s_new s i AddToTail 1 pr]
                         "n_set" -> [n_set i pr]
                         "n_set_p" -> [n_set i pr]
                         "rest" -> []
                         _ -> error "e_osc:m_on:type"
                m_off = case e_type e of
                         "s_new" -> [n_set i [("gate",0)]]
                         "s_new_p" -> []
                         "n_set" -> [n_set i [("gate",0)]]
                         "n_set_p" -> []
                         "rest" -> []
                         _ -> error "e_osc:m_off:type"
            in Just (Bundle (UTCr t) m_on,Bundle (UTCr (t+rt)) m_off)

-- dt = delta-time
e_play :: (Transport t) => t -> [Int] -> [Event] -> IO ()
e_play fd lj le = do
  let act _ _ [] = return ()
      act _ [] _ = error "e_play:id?"
      act t (j:js) (e:es) =
          do let dt = e_fwd e
             case e_osc t j e of
               Just (p,q) -> do case e_instrument_def e of
                                  Just d -> async fd (d_recv d) >> return ()
                                  Nothing -> return ()
                                send fd p
                                send fd q
               Nothing -> return ()
             pauseThreadUntil (t + dt)
             act (t + dt) js es
  st <- utcr
  act st lj le

instance Audible (P Event) where
    play fd = e_play fd [1000..] . unP

instance Audible (Synthdef,P Event) where
    play fd (s,p) = do
      let i_d = InstrumentDef s
          i_nm = InstrumentName (synthdefName s)
          i = pcons i_d (pn (return i_nm) inf)
      _ <- async fd (d_recv s)
      e_play fd [1000..] (unP (pinstr i p))

instance Audible (String,P Event) where
    play fd (s,p) =
        let i = InstrumentName s
        in e_play fd [1000..] (unP (pinstr (return i) p))

