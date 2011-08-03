{-# Language GeneralizedNewtypeDeriving #-}
module Sound.SC3.Lang.Pattern.List where

import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import Data.Foldable as F
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Sound.SC3.Lang.Collection.SequenceableCollection as C
import Sound.SC3.Lang.Math.Pitch as P
import System.Random

-- * P type

newtype P a = P {unP :: [a]}
    deriving (Eq,Functor,Monoid,Foldable,Traversable,Monad,Show)

instance Applicative P where
    pure = P . repeat
    P f <*> P e = P (map (\(f',e') -> f' e') (zip f e))

pnull :: P a -> Bool
pnull = null . toList

psep :: P a -> (P a,P a)
psep (P p) =
    case p of
      [] -> (P [],P [])
      [e] -> (P [e],P [])
      (e:p') -> (P [e],P p')

pfilter :: (a -> Bool) -> P a -> P a
pfilter f = P . filter f . toList

fromList :: [a] -> P a
fromList = P

-- COMMON (to List and Parallel)

-- | Patterns are numbers
instance (Num a) => Num (P a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

-- | Patterns are fractional
instance (Fractional a) => Fractional (P a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = return . fromRational

-- | A very large positive integer
inf :: Int
inf = maxBound

-- | Array choose
achoose :: RandomGen g => g -> A.Array Int a -> [a]
achoose g r =
    let (i,g') = randomR (A.bounds r) g
        x = r A.! i
    in x : achoose g' r

-- | Functor bool
fbool :: (Functor f, Ord a, Num a) => f a -> f Bool
fbool = fmap (> 0)

-- | Foldable choose
fchoose :: (Foldable f,Monoid a,Enum e) => e -> f a -> a
fchoose e p =
    let g = mkStdGen (fromEnum e)
        l = toList p
    in mconcat (achoose g (A.listArray (0,length l - 1) l))

-- | Foldable rand
frand :: (Foldable f,Monoid b,Enum a) => a -> f b -> Int -> b
frand s p n =
    let g = mkStdGen (fromEnum s)
        l = toList p
        q = achoose g (A.listArray (0, length l - 1) l)
    in mconcat (take n q)

-- | Applicative if
aif ::  Applicative f => (a -> Bool) -> f a -> f d -> f d -> f d
aif f = liftA3 (\x z y -> if f x then y else z)

-- | Applicative zip
azip ::  Applicative f => f a -> f b -> f (a,b)
azip = liftA2 (,)

-- | Applicative degreeToKey
adegreeToKey :: (Applicative p,RealFrac a) => p a -> p [a] -> p a -> p a
adegreeToKey = liftA3 P.degree_to_key

-- | Applicative stutter
astutter :: (Monoid (m n),Monoid (m a),Applicative m,Num n,Monad m) =>
            m n -> m a -> m a
astutter ns = join . liftA2 mreplicate (mcycle ns)

-- | Monoidal n-of
mn :: (Monoid a,Num n) => a -> n -> a
mn p n = if n == 0 then mempty else p `mappend` (mn p (n - 1))

-- | Monadic cons
mcons :: (Monoid (m a),Monad m) => a -> m a -> m a
mcons e p = return e `mappend` p

-- | Monoidal cycle
mcycle :: Monoid a => a -> a
mcycle a = a `mappend` mcycle a

-- | Monadic replicate
mreplicate :: (Monoid (m a),Num n,Monad m) => n -> a -> m a
mreplicate n a = if n == 0 then mempty else a `mcons` mreplicate (n-1) a

-- | Monadic null (Eq constraint)
mnull :: (Monoid a,Eq a) => a -> Bool
mnull e = e == mempty

-- | Monadic switch
mswitch :: Monad m => [m b] -> m Int -> m b
mswitch l i = i >>= (l !!)

-- | Monoidal seq
mseq :: Monoid b => [b] -> Int -> b
mseq ps n = mconcat (mconcat (replicate n ps))

phead :: P a -> P a
phead = fst . psep

ptail :: P a -> P a
ptail = snd . psep

ptake :: Int -> P a -> P a
ptake n p =
    if n > 0
    then let (h,t) = psep p
         in h `mappend` ptake (n - 1) t
    else mempty

pdrop :: Int -> P a -> P a
pdrop n p = if n > 0 then pdrop (n - 1) (ptail p) else p

psep' :: P a -> P (a, P a)
psep' p =
    let (x,xs) = psep p
    in x >>= \x' -> return (x',xs)

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost =
    let f i p = if mnull p
                then return i
                else do (x,xs) <- psep' p
                        let r = return i `mappend` f 0 xs
                        if not x then f (i + 1) xs else r
    in ptail . f 0

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in astutter r p

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre =
    let f i p = if mnull p
                then if i == 0 then mempty else return i
                else do (x,xs) <- psep' p
                        let r = return i `mappend` f 0 xs
                        if x then r else f (i + 1) xs
    in f 0

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = mreplicate i Nothing `mappend` return (Just x)
    in join (liftA2 f r q)

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd =
    let f i p = let (h,t) = psep p
                in if mnull h
                   then mempty
                   else let t' = f (Just h) t
                        in case i of
                          Nothing -> h `mappend` t'
                          Just j -> if j == h then t' else h `mappend` t'
    in f Nothing

pser :: [P a] -> Int -> P a
pser ps n = ptake n (mseq ps inf)

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = fromList (C.geom n i s)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = fromList (C.series n i s)

pinterleave :: Eq a => P a -> P a -> P a
pinterleave p q =
    let ((a,p'),(b,q')) = (psep p,psep q)
    in if mnull b
       then p
       else if mnull a
            then q
            else a `mappend` b `mappend` pinterleave p' q'

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pswitch1 :: Eq a => [P a] -> P Int -> P a
pswitch1 ps is =
    if mnull is
    then mempty
    else do (i,j) <- psep' is
            let (l,r) = splitAt i ps
                p = head r
                x = phead p
                ps' = l ++ [ptail p] ++ tail r
            if mnull p then pswitch1 ps j else x `mappend` pswitch1 ps' j

prand_b :: (Eq a,Random a) => StdGen -> P (a,a) -> P a
prand_b g i =
    if mnull i
    then mempty
    else do (h,t) <- psep' i
            let (x,g') = randomR h g
            return x `mappend` prand_b g' t

pwhite :: (Enum e,Random a,Eq a) => e -> P a -> P a -> P a
pwhite n l r =
    let b = azip (mcycle l) (mcycle r)
        g = mkStdGen (fromEnum n)
    in prand_b g b

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

pexprand :: (Enum n,Random a,Floating a) => n -> P a -> P a -> P a
pexprand n l r = liftA3 to_exprand (mcycle l) (mcycle r) (pwhite n l r)

pindex :: P a -> Int -> P a
pindex p n = phead (pdrop n p)

wlookup :: (Ord n,Fractional n) => P a -> [n] -> n -> P a
wlookup x w i = x `pindex` fromJust (C.windex w i)

pwrand :: (Enum e, Random n, Ord n, Fractional n) => e -> P a -> [n] -> P a
pwrand n x w = join (fmap (wlookup x w) (pwhite n 0 1))

psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p = (ptake n p,pdrop n p)

-- * P specialised

pcons :: a -> P a -> P a
pcons = mcons

ppure :: a -> P a
ppure = pure

prepeat :: a -> P a
prepeat = pure

papply :: P (a -> b) -> P a -> P b
papply = (<*>)

pappend :: P a -> P a -> P a
pappend = mappend

pmap :: (a -> b) -> P a -> P b
pmap = fmap

preturn :: a -> P a
preturn = return

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith = liftA2

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 = liftA3

pzip :: P a -> P b -> P (a, b)
pzip = azip

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fbool

pnil :: P a
pnil = mempty

pcycle :: P a -> P a
pcycle = mcycle

pn :: P a -> Int -> P a
pn = mn

preplicate :: Int -> a -> P a
preplicate = mreplicate

pstutter :: P Int -> P a -> P a
pstutter = astutter

pswitch :: [P a] -> P Int -> P a
pswitch = mswitch

pchoose :: Enum e => e -> [P a] -> P a
pchoose = fchoose

prand :: Enum e => e -> [P a] -> Int -> P a
prand = frand

pif :: (a -> Bool) -> P a -> P b -> P b -> P b
pif = aif

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = adegreeToKey

pseq :: [P a] -> Int -> P a
pseq = mseq

pempty :: P a
pempty = mempty

-- * _c

pzipWith_c :: (a -> b -> c) -> P a -> P b -> P c
pzipWith_c f ip iq =
    let go l r p q =
            let l' = l || pnull p
                r' = r || pnull q
                p' = if pnull p then ip else p
                q' = if pnull q then iq else q
            in if l' && r
               then mempty
               else let (ph,pt) = psep p'
                    in if r' && l
                       then mempty
                       else let (qh,qt) = psep q'
                            in do ph' <- ph
                                  qh' <- qh
                                  f ph' qh' `pcons` go l' r' pt qt
    in go False False ip iq

instance Extending P where
    zipWith_c = pzipWith_c
