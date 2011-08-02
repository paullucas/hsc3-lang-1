module Sound.SC3.Lang.Pattern.Parallel where

import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Traversable as T
import qualified Sound.SC3.Lang.Collection.SequenceableCollection as S
import qualified Sound.SC3.Lang.Math.Pitch as S
import System.Random

-- * Patterns

data P a = Nil | Unit a | Seq (P a) (P a) | Par (P a) (P a)
           deriving (Eq,Show)

pnull :: P a -> Bool
pnull p =
    case p of
      Nil -> True
      _ -> False

preturn :: a -> P a
preturn = Unit -- Seq . Unit

-- left hand side of Seq ought not be Seq or Nil
pappend :: P a -> P a -> P a
pappend p q =
    case p of
      Nil -> q
      Seq r s -> Seq r (s `pappend` q)
      _ -> Seq p q

pnil :: P a
pnil = Nil

instance Monoid (P a) where
    mempty = Nil
    mappend = pappend

pfoldMap :: Monoid b => (a -> b) -> P a -> b
pfoldMap f p =
    case p of
      Nil -> mempty
      Unit x -> f x
      Seq x y -> pfoldMap f x `mappend` pfoldMap f y
      Par x y -> pfoldMap f x `mappend` pfoldMap f y

instance F.Foldable P where
    foldMap = pfoldMap

instance T.Traversable P where
    traverse f p =
        case p of
          Nil -> pure Nil
          Unit x -> Unit <$> f x
          Seq a b -> Seq <$> T.traverse f a <*> T.traverse f b
          Par a b -> Par <$> T.traverse f a <*> T.traverse f b

instance Monad P where
    m >>= k = F.foldr (pappend . k) mempty m
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

pmap :: (a -> b) -> P a -> P b
pmap f p =
    case p of
      Nil -> Nil
      Unit e -> Unit (f e)
      Seq e e' -> Seq (pmap f e) (pmap f e')
      Par e e' -> Par (pmap f e) (pmap f e')

instance Functor P where
    fmap = pmap

ppure :: a -> P a
ppure e = Seq (Unit e) (ppure e)

papply :: P (a -> b) -> P a -> P b
papply f p =
    let f' = shp p f
    in case (f',p) of
         (Nil,_) -> Nil
         (Unit f'',Unit p') -> Unit (f'' p')
         (Seq f'' f''',Seq p' p'') -> Seq (papply f'' p') (papply f''' p'')
         (Par f'' f''',Par p' p'') -> Par (papply f'' p') (papply f''' p'')
         _ -> error (show ("papply",pmap (const ()) f',pmap (const ()) p))

instance Applicative P where
    pure = ppure
    (<*>) = papply

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

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P a -> P b -> P c
pzipWith f = liftA2 f

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f = liftA3 f

pzip :: P a -> P b -> P (a, b)
pzip = pzipWith (,)

instance (Num a) => Num (P a) where
    (+) = pzipWith (+)
    (-) = pzipWith (-)
    (*) = pzipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = preturn . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional (P a) where
    (/) = pzipWith (/)
    recip = fmap recip
    fromRational = preturn . fromRational

-- * Pattern constructors

-- | A very large positive integer
inf :: Int
inf = 83886028

pseq :: [P a] -> Int -> P a
pseq ps n =
    let ps' = concat (replicate n ps)
    in foldr mappend mempty ps'

prepeat :: a -> P a
prepeat = pure

pn :: P a -> Int -> P a
pn p n = if n == 0 then mempty else p `mappend` (pn p (n - 1))

preplicate :: Int -> a -> P a
preplicate n a = if n == 0 then Nil else Seq (Unit a) (preplicate (n - 1) a)

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

phead :: P a -> P a
phead = fst . psep

ptail :: P a -> P a
ptail = snd . psep

ptake :: Int -> P a -> P a
ptake n p =
    if n > 0
    then let (h,t) = psep p
         in h `pappend` ptake (n - 1) t
    else Nil

pdrop :: Int -> P a -> P a
pdrop n p = if n > 0 then pdrop (n - 1) (ptail p) else p

pser :: [P a] -> Int -> P a
pser ps n = ptake n (pseq ps inf)

pbool :: (Functor f, Ord a, Num a) => f a -> f Bool
pbool = fmap (> 0)

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd =
    let f i p = let (h,t) = psep p
                in if pnull h
                   then Nil
                   else let t' = f (Just h) t
                        in case i of
                          Nothing -> h `pappend` t'
                          Just j -> if j == h then t' else h `pappend` t'
    in f Nothing

pcycle :: P a -> P a
pcycle a = a `pappend` pcycle a

pstutter :: P Int -> P a -> P a
pstutter ns = join . pzipWith preplicate (pcycle ns)

pindex :: P a -> Int -> P a
pindex p n = phead (pdrop n p)

pswitch' :: P (P a) -> P Int -> P a
pswitch' p = join . pzipWith pindex p

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p = (ptake n p,pdrop n p)

unp :: P a -> a
unp p =
    case p of
      Unit x -> x
      _ -> error "unp"

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

-- | Choose elements from an array at random
choosea :: StdGen -> A.Array Int a -> [a]
choosea g r =
    let (i, g') = randomR (A.bounds r) g
        x = r A.! i
    in x : choosea g' r

pchoose :: Enum e => e -> [P a] -> P a
pchoose e p =
    let g = mkStdGen (fromEnum e)
    in mconcat (choosea g (A.listArray (0, length p - 1) p))

prand :: Enum n => n -> [P a] -> Int -> P a
prand s ps n =
    let g = mkStdGen (fromEnum s)
        qs = choosea g (A.listArray (0, length ps - 1) ps)
    in foldr mappend mempty (take n qs)

-- | infinite monadic recursion
mrec :: Monad m => (a -> m a) -> a -> m a
mrec f i = f i >>= mrec f

-- | recursion function useful for random processes (g = random state)
r_chain :: g -> (g -> (a,g)) -> P a
r_chain g f =
    let (r,g') = f g
    in Unit r `pappend` r_chain g' f

prand_b :: (RandomGen g,Random a) => g -> P (a,a) -> P a
prand_b g i =
    if pnull i
    then mempty
    else let (h,t) = psep i
             (x,g') = randomR (unp h) g
         in return x `mappend` prand_b g' t

pwhite :: (Enum e,Random a) => e -> P a -> P a -> P a
pwhite e l r =
    let b = pzip (pcycle l) (pcycle r)
        g = mkStdGen (fromEnum e)
    in prand_b g b

to_exprand :: (Floating b) => b -> b -> b -> b
to_exprand l r i = l * (log (r / l) * i)

pexprand :: (Enum e,Random a,Floating a) => e -> P a -> P a -> P a
pexprand e l r = pzipWith3 to_exprand (pcycle l) (pcycle r) (pwhite e l r)

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost =
    let f i p = if pnull p
                then Unit i
                else let (x,xs) = psep p
                     in if not (unp x)
                        then f (i + 1) xs
                        else Unit i `pappend` f 0 xs
    in ptail . f 0

pclutch :: P a -> P Bool -> P a
pclutch p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter r p

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre =
    let f i p = if pnull p
                then if i == 0 then Nil else Unit i
                else let (x,xs) = psep p
                     in if unp x
                        then Unit i `pappend` f 0 xs
                        else f (i + 1) xs
    in f 0

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing `mappend` return (Just x)
    in join (pzipWith f r q)

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 S.degree_to_key

pfilter :: (a -> Bool) -> P a -> P a
pfilter f p =
    case p of
      Nil -> Nil
      Unit e -> if f e then Unit e else Nil
      Seq a b -> pfilter f a `pappend` pfilter f b
      Par a b -> pfilter f a `Par` pfilter f b

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pcons :: a -> P a -> P a
pcons e p = Unit e `pappend` p

fromList :: [a] -> P a
fromList l =
    case l of
      [] -> Nil
      (e:l') -> e `pcons` fromList l'

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n = fromList (S.geom n i s)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n = fromList (S.series n i s)

pinterleave :: P a -> P a -> P a
pinterleave p q =
    case (psep p,psep q) of
      (_,(Nil,_)) -> p
      ((Nil,_),_) -> q
      ((a,p'),(b,q')) -> a `pappend` b `pappend` pinterleave p' q'
