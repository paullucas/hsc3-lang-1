{-# Language FlexibleInstances #-}
module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Control.Monad
import Data.Foldable as F
import qualified Data.List as L
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

-- in order for mappend to be productive in mconcat on an infinite
-- list it cannot store the right-hand stop/continue mode
instance Monoid (P a) where
--    mappend (P xs _) (P ys st) = P (xs ++ ys) st
    p `mappend` q = fromList (unP p ++ unP q)
    mempty = P [] Continue

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


inf :: Int
inf = maxBound

-- * Extension

stP_join :: [M] -> M
stP_join m = if L.any (== Stop) m then Stop else Continue

pextension :: [P a] -> [()]
pextension x =
    let x' = filter ((== Stop) . stP) x
    in if null x'
       then C.extension (map toList x)
       else C.extension (map toList x')

--pextend :: [P a] -> [P a]
--pextend l = zipWith (\_ x -> x) (pextension l) (map pcycle l)

ptranspose :: [P a] -> P [a]
ptranspose l =
    let d = L.transpose (map unP l)
        s = stP_join (map stP l)
    in P d s

pflop :: [P a] -> P [a]
pflop l =
    let l' = map pcycle l
    in pzipWith (\_ x -> x) (P (pextension l) Stop) (ptranspose l')

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

stopping :: P a -> P a
stopping (P xs _) = P xs Stop

stoppingN :: Int -> P a -> P a
stoppingN n (P xs _) = P xs (if n == inf then Continue else Stop)

continuing :: P a -> P a
continuing (P xs _) = P xs Continue

fromList :: [a] -> P a
fromList xs = P xs Continue

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

padd :: Num a => Key -> P a -> P (Event a) -> P (Event a)
padd k p = pzipWith (\i j -> e_edit k (+ i) j) p

pbind1 :: (String,P a) -> P (Event a)
pbind1 (k,P xs st) =
    let xs' = map (\i -> e_from_list [(k,i)]) xs
    in P xs' st

pbind :: [(String,P a)] -> P (Event a)
pbind xs =
    let xs' = fmap (\(k,v) -> pzip (return k) v) xs
    in fmap e_from_list (pflop xs')

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

pedit :: Key -> (a -> a) -> P (Event a) -> P (Event a)
pedit k f = fmap (e_edit k f)

pexprand :: (Enum e,Random a,Floating a) => e -> a -> a -> Int -> P a
pexprand e l r n = fmap (N.exprand l r) (pwhite e l r n)

pfinval :: Int -> P a -> P a
pfinval = ptake

pfold :: (RealFrac n) => P n -> n -> n -> P n
pfold p i j = fmap (\n -> fold_ n i j) p

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

place :: [[a]] -> Int -> P a
place a n =
    let i = length a
        f = if n == inf then id else take (n * i)
    in stoppingN n (fromList (f (L.concat (C.flop a))))

ppatlace :: [P a] -> Int -> P a
ppatlace a n =
    let i = length a
        f = if n == inf then id else take (n * i)
    in stoppingN n (P (f (L.concat (C.flop (map unP a)))) Continue)

pn :: P a -> Int -> P a
pn = flip pconcatReplicate

pnormalizeSum :: Fractional n => P n -> P n
pnormalizeSum = liftP C.normalizeSum

prand' :: Enum e => e -> [P a] -> P a
prand' e a =
    let a' = map unP a
        k = length a - 1
        f g = let (i,g') = randomR (0,k) g
              in (a' !! i) ++ f g'
    in P (f (mkStdGen (fromEnum e))) Continue

prand :: Enum e => e -> [P a] -> Int -> P a
prand e a n = ptake n (prand' e a)

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

pseq :: [P a] -> Int -> P a
pseq a i = stoppingN i (pn (pconcat a) i)

pseq' :: [Int] -> [P a] -> Int -> P a
pseq' n q =
    let go _ 0 = pempty
        go p c = let (i,j) = unzip (zipWith psplitAt n p)
                 in pconcat i `pappend` go j (c - 1)
    in go (map pcycle q)

pser :: [P a] -> Int -> P a
pser a i = ptake i (pcycle (pconcat a))

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = P (C.series n i s) Stop

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
    in go (M.fromList (zip [0..] ps))

pswitch1 :: [P a] -> P Int -> P a
pswitch1 l = liftP (switch1 (map unP l))

ptuple :: [P a] -> Int -> P [a]
ptuple p = pseq [pflop p]

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

pappend :: P a -> P a -> P a
pappend = mappend

pconcat :: [P a] -> P a
pconcat = mconcat

pempty :: P a
pempty = mempty

-- * Monad aliases

pjoin :: P (P a) -> P a
pjoin = join

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

-- * Pattern audition

-- t = time, i = node id, s = instrument name
-- rt = release time, a = parameters
e_osc :: (Fractional n,Real n) =>
         Double -> Int -> String -> Event n -> (OSC,OSC)
e_osc t i s e =
    let rt = e_sustain' e
        f = e_freq e
        a = ("freq",f) : e_arg e
    in (Bundle (UTCr t) [s_new s i AddToTail 1 a]
       ,Bundle (UTCr (t+rt)) [n_set i [("gate",0)]])

-- dt = delta-time
e_play :: (Transport t, Real n, Fractional n) =>
          t -> [String] -> [Event n] -> IO ()
e_play fd ls le = do
  let act _ _ _ [] = return ()
      act _ _ [] _ = return ()
      act _ [] _ _ = error "e_play: no id?"
      act t (i:is) (s:ss) (e:es) = do
                  let dt = e_dur' e
                      (p,q) = e_osc t i s e
                  send fd p
                  send fd q
                  pauseThreadUntil (t + dt)
                  act (t + dt) is ss es
  st <- utcr
  act st [100..] ls le

instance (Real n, Fractional n) => Audible (P (Event n)) where
    play fd = e_play fd (repeat "default") . unP

instance (Real n, Fractional n) => Audible (String,P (Event n)) where
    play fd (s,p) = e_play fd (repeat s) (unP p)

instance (Real n, Fractional n) => Audible (P (String,Event n)) where
    play fd p =
        let (s,e) = unzip (unP p)
        in e_play fd s e
