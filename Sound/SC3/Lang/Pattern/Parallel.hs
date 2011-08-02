module Sound.SC3.Lang.Pattern.Parallel where

import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Traversable as T
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
import qualified Sound.SC3.Lang.Math.Pitch as P
import System.Random

-- * P type

data P a = Nil | Unit a | Seq (P a) (P a) | Par (P a) (P a)
           deriving (Eq,Show)

pnil :: P a
pnil = Nil

pnull :: P a -> Bool
pnull p =
    case p of
      Nil -> True
      _ -> False

unp :: P a -> a
unp p =
    case p of
      Unit x -> x
      _ -> error "unp"

psep :: P a -> (P a,P a)
psep i =
    case i of
      Nil -> (Nil,Nil)
      Unit _ -> (i,Nil)
      Seq (Seq _ _) _ -> error "psep"
      Seq p q -> (p,q)
      Par p q -> let (p',p'') = psep p
                     (q',q'') = psep q
                 in (Par p' q',Par p'' q'')

pfilter :: (a -> Bool) -> P a -> P a
pfilter f p =
    case p of
      Nil -> Nil
      Unit e -> if f e then Unit e else Nil
      Seq a b -> pfilter f a `mappend` pfilter f b
      Par a b -> pfilter f a `Par` pfilter f b

fromList :: [a] -> P a
fromList l =
    case l of
      [] -> Nil
      (e:l') -> e `pcons` fromList l'

-- left hand side of Seq ought not be Seq or Nil
instance Monoid (P a) where
    mempty = Nil
    mappend p q =
        case p of
          Nil -> q
          Seq r s -> Seq r (s `mappend` q)
          _ -> Seq p q

instance F.Foldable P where
    foldMap f p =
        case p of
          Nil -> mempty
          Unit x -> f x
          Seq x y -> F.foldMap f x `mappend` F.foldMap f y
          Par x y -> F.foldMap f x `mappend` F.foldMap f y

instance T.Traversable P where
    traverse f p =
        case p of
          Nil -> pure Nil
          Unit x -> Unit <$> f x
          Seq a b -> Seq <$> T.traverse f a <*> T.traverse f b
          Par a b -> Par <$> T.traverse f a <*> T.traverse f b

instance Monad P where
    m >>= k = F.foldr (mappend . k) mempty m
    return = Unit

shp :: P a -> P b -> P b
shp a b =
    case (a,b) of
      (Nil,_) -> Nil
      (Unit _,Unit _) -> b
      (Seq p p',Unit _) -> Seq (shp p b) (shp p' b)
      (Seq p p',Seq q q') -> Seq (shp p q) (shp p' q')
      (Par p p',Unit _) -> Par (shp p b) (shp p' b)
      (Par p p',Seq _ _) -> Par (shp p b) (shp p' b)
      (Par p p',Par q q') -> Par (shp p q) (shp p' q')
      (_,Nil) -> Nil
      (Unit _,Seq q _) -> shp a q
      (Unit _,Par q _) -> shp a q
      (Seq _ _,Par q _) -> shp a q

instance Functor P where
    fmap f p =
        case p of
          Nil -> Nil
          Unit e -> Unit (f e)
          Seq e e' -> Seq (pmap f e) (pmap f e')
          Par e e' -> Par (pmap f e) (pmap f e')

instance Applicative P where
    pure e = Seq (Unit e) (pure e)
    f <*> p =
        let f' = shp p f
        in case (f',p) of
             (Nil,_) -> Nil
             (Unit f'',Unit p') -> Unit (f'' p')
             (Seq f'' f''',Seq p' p'') -> Seq (f'' <*> p') (f''' <*> p'')
             (Par f'' f''',Par p' p'') -> Par (f'' <*> p') (f''' <*> p'')
             _ -> error (show ("<*>",pmap (const ()) f',pmap (const ()) p))

{-
let p0 = pseq [1,2,3] 1
let p0' = shp p0 1
let p1 = pseq (map Unit [1,3..11]) 1
let p1' = shp p1 (pseq (map Unit [1..11]) 1)
let p2 = Par (pseq [1,2,3] 1) 4
let p2' = shp p2 (pseq [1,2] 1)
let p3 = Par 1 (Par 2 (pseq [3,4,5] 1))
let p3' = shp p3 (pseq [6,7,8,9] 1)
let f = pmap negate
(p0,p0',f p0,f p0')
(p1,p1',f p1,f p1')
(p2,p2',f p2,f p2')
(p3,p3',f p3,f p3')
let shp' (a,b) = (a,shp a b)
let p4 = shp' (1,return negate)
let p5 = shp' (pseq [3,4,5] 1,preturn negate)
let p6 = shp' (Par 1 (Par 2 (pseq [3,4,5] 1)),ppure negate)
let h (i,j) = papply j i
let s = pmap (const ())
let s' (p,q) = (s p,s q)
h p4
h p5
h p6
s' p6
-}

-- * Pattern functions

-- * Extension (list & pattern)

class Extending f where
    zipWith_c :: (a -> b -> c) -> f a -> f b -> f c

pzipWith_c :: (a -> b -> c) -> P a -> P b -> P c
pzipWith_c f ip iq =
    let go l r p q =
            let l' = l || pnull p
                r' = r || pnull q
                p' = if pnull p then ip else p
                q' = if pnull q then iq else q
            in if l' && r
               then pnil
               else let (ph,pt) = psep p'
                    in if r' && l
                       then pnil
                       else let (qh,qt) = psep q'
                            in f (unp ph) (unp qh) `pcons` go l' r' pt qt
    in go False False ip iq

instance Extending P where
    zipWith_c = pzipWith_c

(+.) :: (Extending f,Num a) => f a -> f a -> f a
(+.) = zipWith_c (+)

(*.) :: (Extending f,Num a) => f a -> f a -> f a
(*.) = zipWith_c (*)

(/.) :: (Extending f,Fractional a) => f a -> f a -> f a
(/.) = zipWith_c (/)

(-.) :: (Extending f,Num a) => f a -> f a -> f a
(-.) = zipWith_c (-)

-- * Common

instance (Num a) => Num (P a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional (P a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = return . fromRational

ppure :: a -> P a
ppure = pure

prepeat :: a -> P a
prepeat = pure

papply :: P (a -> b) -> P a -> P b
papply = (<*>)

pmap :: (a -> b) -> P a -> P b
pmap = fmap

preturn :: a -> P a
preturn = return

pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith = liftA2

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 = liftA3

pzip :: P a -> P b -> P (a, b)
pzip = pzipWith (,)

-- | A very large positive integer
inf :: Int
inf = 83886028

-- | Choose an element of an array at random
choosea :: StdGen -> A.Array Int a -> [a]
choosea g r =
    let (i, g') = randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

rrand :: (Random a, Enum e) => e -> a -> a -> a
rrand e a b =
    let g = mkStdGen (fromEnum e)
    in fst (randomR (a,b) g)

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

fbool :: (Functor f, Ord a, Num a) => f a -> f Bool
fbool = fmap (> 0)

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fbool

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

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost =
    let f i p = if pnull p
                then return i
                else let (x,xs) = psep p
                     in if not (unp x)
                        then f (i + 1) xs
                        else return i `mappend` f 0 xs
    in ptail . f 0

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre =
    let f i p = if pnull p
                then if i == 0 then mempty else return i
                else let (x,xs) = psep p
                     in if unp x
                        then return i `mappend` f 0 xs
                        else f (i + 1) xs
    in f 0

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing `mappend` return (Just x)
    in join (pzipWith f r q)

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 P.degree_to_key

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd =
    let f i p = let (h,t) = psep p
                in if pnull h
                   then mempty
                   else let t' = f (Just h) t
                        in case i of
                          Nothing -> h `mappend` t'
                          Just j -> if j == h then t' else h `mappend` t'
    in f Nothing

pser :: [P a] -> Int -> P a
pser ps n = ptake n (pseq ps inf)

pcycle :: P a -> P a
pcycle a = a `mappend` pcycle a

pstutter :: P Int -> P a -> P a
pstutter ns = join . pzipWith preplicate (pcycle ns)

preplicate :: Int -> a -> P a
preplicate n = fromList . replicate n

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = fromList (C.geom n i s)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = fromList (C.series n i s)

pinterleave :: P a -> P a -> P a
pinterleave p q =
    let ((a,p'),(b,q')) = (psep p,psep q)
    in if pnull b
       then p
       else if pnull a
            then q
            else a `mappend` b `mappend` pinterleave p' q'

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pn :: P a -> Int -> P a
pn p n = if n == 0 then mempty else p `mappend` (pn p (n - 1))

pseq :: [P a] -> Int -> P a
pseq ps n =
    let ps' = concat (replicate n ps)
    in foldr mappend mempty ps'

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pswitch1 :: [P a] -> P Int -> P a
pswitch1 ps is =
    if pnull is
    then mempty
    else let (i,j) = psep is
             (l,r) = splitAt (unp i) ps
             p = head r
         in if pnull p
            then pswitch1 ps j
            else let x = phead p
                     ps' = l ++ [ptail p] ++ tail r
                 in x `mappend` pswitch1 ps' j

pchoose :: Enum e => e -> [P a] -> P a
pchoose e p =
    let g = mkStdGen (fromEnum e)
    in mconcat (choosea g (A.listArray (0, length p - 1) p))

prand :: Enum e => e -> [P a] -> Int -> P a
prand s ps n =
    let g = mkStdGen (fromEnum s)
        qs = choosea g (A.listArray (0, length ps - 1) ps)
    in foldr mappend mempty (take n qs)

prand_b :: (Random a) => StdGen -> P (a,a) -> P a
prand_b g i =
    if pnull i
    then mempty
    else let (h,t) = psep i
             (x,g') = randomR (unp h) g
         in return x `mappend` prand_b g' t

pwhite :: (Enum e,Random a) => e -> P a -> P a -> P a
pwhite n l r =
    let b = pzip (pcycle l) (pcycle r)
        g = mkStdGen (fromEnum n)
    in prand_b g b

pexprand :: (Enum n,Random a,Floating a) => n -> P a -> P a -> P a
pexprand n l r = pzipWith3 to_exprand (pcycle l) (pcycle r) (pwhite n l r)

windex :: (Ord a, Num a) => [a] -> a -> Int
windex w n = fromJust (findIndex (n <) (C.integrate w))

pindex :: P a -> Int -> P a
pindex p n = phead (pdrop n p)

wlookup :: (Ord n,Fractional n) => P a -> [n] -> n -> P a
wlookup x w i = x `pindex` windex w i

pwrand :: (Enum e, Random n, Ord n, Fractional n) => e -> P a -> [n] -> P a
pwrand n x w = join (fmap (wlookup x w) (pwhite n 0 1))

{-
pwrand' :: (Enum n) => n -> P b -> [Double] -> P b
pwrand' n x w = map (wlookup x w) (pwhite n 0 1)
-}

pif :: (a -> Bool) -> P a -> P b -> P b -> P b
pif f = pzipWith3 (\x z y -> if f x then y else z)

psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p = (ptake n p,pdrop n p)

pcons :: a -> P a -> P a
pcons e p = return e `mappend` p
