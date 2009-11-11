{-# LANGUAGE ExistentialQuantification #-}

module Sound.SC3.Lang.Pattern.Step where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.IntMap as M
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Monoid as M
import qualified Data.Traversable as T
import qualified Sound.SC3.Lang.Math.Pitch as S
import qualified System.Random as R

data P a = Empty
         | Value a
         | RP (R.StdGen -> (P a, R.StdGen))
         | Fix R.StdGen (P a)
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
step g (RP f) =
    let (p, g') = f g
    in step g' p
step g (Fix g' p) =
    case step g' p of
      Done _ -> Done g
      Result g'' a p' -> Result g a (Fix g'' p')
step g (Append x y) =
    case step g x of
      Done g' -> step g' y
      Result g' a x' -> Result g' a (Append x' y)
step g (Continue p f) =
    case step g p of
      Done g' -> Done g'
      Result g' x p' -> step g' (f x p')
step g (Unfoldr f x) =
    let y = f x
    in case y of
         Nothing -> Done g
         Just (a, x') -> Result g a (Unfoldr f x')
step g (Apply p q) =
    case step g p of
      Done g' -> Done g'
      Result g' f p' -> case step g' q of
                          Done g'' -> Done g''
                          Result g'' x q' -> Result g'' (f x) (Apply p' q')
step g (Scan f f' i p) =
    case step g p of
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

pfix :: Int -> P a -> P a
pfix n = Fix (R.mkStdGen n)

-- * Control

pfilter :: (a -> Bool) -> P a -> P a
pfilter f p =
    let g x p' = if f x
                 then M.mappend (return x) (pfilter f p')
                 else pfilter f p'
    in pcontinue p g

plist :: [P a] -> P a
plist = foldr M.mappend M.mempty

pcons :: a -> P a -> P a
pcons = M.mappend . return

preplicate_ :: Int -> P a -> P a
preplicate_ n p | n > 0 = M.mappend p (preplicate_ (n - 1) p)
                | otherwise = M.mempty

preplicate :: P Int -> P a -> P a
preplicate n p = n >>= (\x -> preplicate_ x p)

pn :: P a -> P Int -> P a
pn = flip preplicate

pn_ :: P a -> Int -> P a
pn_ = flip preplicate_

-- | 'n' initial values at 'p'.
ptake_ :: Int -> P a -> P a
ptake_ n p =
    let e = error "ptake_"
    in pzipWith const p (preplicate_ n (return e))

ptake :: P Int -> P a -> P a
ptake n p =
    let e = error "ptake"
    in pzipWith const p (preplicate n (return e))

-- | 'n' initial values at pcycle of 'p'.
prestrict_ :: Int -> P a -> P a
prestrict_ n = ptake_ n . pcycle

prestrict :: P Int -> P a -> P a
prestrict n = ptake n . pcycle

pmapMaybe :: (a -> Maybe b) -> P a -> P b
pmapMaybe f = fmap M.fromJust . pfilter M.isJust . fmap f

preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

pzipWith3 :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3 f p q = (A.<*>) (A.pure f A.<*> p A.<*> q)

pzip :: P a -> P b -> P (a,b)
pzip = pzipWith (,)

pzip3 :: P a -> P b -> P c -> P (a,b,c)
pzip3 = pzipWith3 (,,)

pseries :: (Num a) => a -> a -> Int -> P a
pseries i s n =
    let f (_, 0) = Nothing
        f (j, m) = Just (return j, (j + s, m - 1))
    in plist (L.unfoldr f (i, n))

pgeom :: (Num a) => a -> a -> Int -> P a
pgeom i s n =
    let f (_, 0) = Nothing
        f (j, m) = Just (return j, (j * s, m - 1))
    in plist (L.unfoldr f (i, n))

pstutter' :: P Int -> P a -> P a
pstutter' n p =
    let f :: Int -> a -> P a
        f i e = preplicate (return i) (return e)
    in psequence (pzipWith f n p)

pstutter :: P Int -> P a -> P a
pstutter = pstutter' . pcycle

-- | Count false values preceding each true value.
pcountpre :: P Bool -> P Int
pcountpre p =
    let f x e = if e then (0, Just x) else (x + 1, Nothing)
    in pmapMaybe id (pscan f Nothing 0 p)

-- | Count false values following each true value.
pcountpost :: P Bool -> P Int
pcountpost p =
    let f x e = if e then (0, Just x) else (x + 1, Nothing)
    in ptail (pmapMaybe id (pscan f (Just Just) 0 p))

pclutch' :: P a -> P Bool -> P a
pclutch' p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter' r p

pbool :: (Ord a, Num a) => P a -> P Bool
pbool = fmap (> 0)

pclutch :: (Num b, Ord b) => P a -> P b -> P a
pclutch p = pclutch' p . pbool

pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a
pdegreeToKey = pzipWith3 S.degree_to_key

pfin :: P Int -> P a -> P a
pfin = ptake

pfin_ :: Int -> P a -> P a
pfin_ = ptake_

wrap :: (Ord a, Num a) => a -> a -> a -> a
wrap l r x = if x > r
             then wrap l r (x - (r - l))
             else if x < l
                  then wrap l r (x + (r - l))
                  else x

pwrap :: (Ord a, Num a) => P a -> P a -> P a -> P a
pwrap x l r =
    let f x' l' r' = wrap l' r' x'
    in pzipWith3 f x (pcycle l) (pcycle r)

-- | Remove successive duplicates.
prsd :: (Eq a) => P a -> P a
prsd p =
    let f Nothing a = (Just a, Just a)
        f (Just x) a = (Just a, if a == x then Nothing else Just a)
    in pmapMaybe id (pscan f Nothing Nothing p)

psequence :: P (P a) -> P a
psequence = M.join

pduple :: (a, a) -> P a
pduple (x, y) = return x `M.mappend` return y

pinterleave :: P a -> P a -> P a
pinterleave p = psequence . fmap pduple . pzip p

ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i = M.mappend (preplicate_ i (return Nothing)) . return . Just
    in M.join (pzipWith f r q)

pif :: P Bool -> P a -> P a -> P a
pif b p q =
    let f (x, y) True = ((ptail x, y), phead x)
        f (x, y) False = ((x, ptail y), phead y)
    in psequence (pscan f Nothing (p,q) b)

phead :: P a -> P a
phead p = pcontinue p (\x _ -> return x)

ptail :: P a -> P a
ptail p = pcontinue p (\_ p' -> p')

pdrop :: P Int -> P a -> P a
pdrop n p = n >>= (\x -> if x > 0
                         then pdrop (return (x-1)) (ptail p)
                         else p)

pscanl :: (a -> y -> a) -> a -> P y -> P a
pscanl f i p =
    let g x y = let r = f x y in (r, r)
    in pcons i (pscan g Nothing i p)

-- * Random


-- Random numbers

prrandf :: (R.Random a) => (a -> a -> a -> a) -> a -> a -> P a
prrandf f l r = prp (\g -> let (x, g') = R.randomR (l,r) g
                           in (return (f l r x), g'))

prrand :: (R.Random a) => a -> a -> P a
prrand = prrandf (\_ _ x -> x)

prrandexp :: (Floating a, R.Random a) => a -> a -> P a
prrandexp = prrandf (\l r x -> l * (log (r / l) * x))

pchoosea :: A.Array Int (P a) -> P a
pchoosea r = prp (\g -> let (i, g') = R.randomR (A.bounds r) g 
                        in (r A.! i, g'))

pchoose :: [P a] -> P a
pchoose l = pchoosea (A.listArray (0, length l - 1) l)

prand :: [P a] -> P Int -> P a
prand p = pseq [pchoose p]

pwhite :: (R.Random a) => P a -> P a -> P Int -> P a
pwhite l r n = prestrict n (M.join (pzipWith prrand l r))

pexprand :: (Floating a, R.Random a) => P a -> P a -> P Int -> P a
pexprand l r n = prestrict n (M.join (pzipWith prrandexp l r))

pxrand :: (Eq a) => [P a] -> P Int -> P a
pxrand p n = ptake n (prsd (pseq [pchoose p] pinf))

pwrand :: [P a] -> [P a] -> P Int -> P a
pwrand = undefined

-- * List


pseq_ :: [P a] -> Int -> P a
pseq_ l n = plist (concat (replicate n l))

pseq :: [P a] -> P Int -> P a
pseq l n = n >>= (\x -> plist (concat (replicate x l)))

-- | 'n' values from the infinite cycle of the streams at l.
pser_ :: [P a] -> Int -> P a
pser_ l n = prestrict_ n (plist l)

pser :: [P a] -> P Int -> P a
pser l n = prestrict n (plist l)

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pswitch1m :: M.IntMap (P a) -> P Int -> P a
pswitch1m m is =
    let f i js = let h = phead (m M.! i)
                     t = ptail (m M.! i)
                 in h `M.mappend` pswitch1m (M.insert i t m) js
    in pcontinue is f

pswitch1 :: [P a] -> P Int -> P a
pswitch1 = pswitch1m . M.fromList . zip [0..]

ppatlace :: [P a] -> P Int -> P a
ppatlace ps n =
    let is = pseq (map return [0 .. length ps - 1]) pinf
    in ptake n (pswitch1 ps is)

{-

Neither the definition above or the variant below are correct.
Both deadlock once all patterns are empty.  pswitch1 has the 
same problem.  

ppatlacea :: P (P a) -> P a
ppatlacea ps = 
    let f p qs = let h = phead p
                     t = ptail p
                     rs = qs `mappend` return t
                 in h `mappend` (ppatlacea rs)
    in pcontinue ps f
-}

-- * Extend

pzipWith_c :: (a -> b -> c) -> P a -> P b -> P c
pzipWith_c f p = pzipWith f p . pcycle

(+.) :: Num a => P a -> P a -> P a
(+.) = pzipWith_c (+)

(*.) :: Num a => P a -> P a -> P a
(*.) = pzipWith_c (*)

(/.) :: Fractional a => P a -> P a -> P a
(/.) = pzipWith_c (/)

(-.) :: Num a => P a -> P a -> P a
(-.) = pzipWith_c (-)
