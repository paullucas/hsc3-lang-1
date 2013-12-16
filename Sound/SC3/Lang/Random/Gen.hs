-- | 'RandomGen' based @sclang@ random number functions.
module Sound.SC3.Lang.Random.Gen where

import qualified Data.DList as DL {- dlist -}
import Data.Maybe {- base  -}
import System.Random {- random -}
import System.Random.Shuffle {- random-shuffle -}

import qualified Sound.SC3.Lang.Collection as C
import Sound.SC3.Lang.Core
import qualified Sound.SC3.Lang.Math as M

-- | @SimpleNumber.rand@ is 'randomR' in (0,/n/).
rand :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand n = randomR (0,n)

-- | State modifying variant of 'iterate'.  Lifts random generator
-- functions to infinte lists.
--
-- > let r = [3,1,7,0,12,1,6,4,12,11,7,4]
-- > in take 12 (r_iterate (rand 12) (mkStdGen 0)) == r
r_iterate :: (t -> (a, t)) -> t -> [a]
r_iterate f g = let (r,g') = f g in r : r_iterate f g'

-- | Function underlying both 'kvariant' and 'kvariant''.
mk_kvariant :: r -> (t -> r -> r) -> (r -> r') -> Int -> (g -> (t,g)) -> g -> (r',g)
mk_kvariant k_nil k_join un_k k f =
    let go x i g = case i of
                     0 -> (un_k x,g)
                     _ -> let (y,g') = f g
                          in go (k_join y x) (i - 1) g'
    in go k_nil k

-- | Construct variant of /f/ generating /k/ values.  Note that the
-- result is the reverse of the initial sequence given by 'r_iterate'.
--
-- > let r = [3,1,7,0,12,1,6,4,12,11,7,4]
-- > in fst (kvariant 12 (rand 12) (mkStdGen 0)) == reverse r
kvariant :: Int -> (g->(a,g)) -> g->([a],g)
kvariant = mk_kvariant [] (:) id

-- | Variant of 'kvariant' that generates sequence in the same order
-- as 'r_iterate'.  There is perhaps a slight overhead from using a
-- difference list.
--
-- > let r = [3,1,7,0,12,1,6,4,12,11,7,4]
-- > in fst (kvariant' 12 (rand 12) (mkStdGen 0)) == r
kvariant' :: Int -> (g->(a,g)) -> g->([a],g)
kvariant' = mk_kvariant DL.empty (flip DL.snoc) DL.toList

-- | Variant of 'rand' generating /k/ values.
--
-- > fst (nrand 10 (5::Int) (mkStdGen 246873)) == [0,5,4,0,4,5,3,2,3,1]
nrand :: (RandomGen g,Random n,Num n) => Int -> n -> g -> ([n],g)
nrand k = kvariant k . rand

-- | @SimpleNumber.rand2@ is 'randomR' in (-/n/,/n/).
rand2 :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand2 n = randomR (-n,n)

-- | Variant of 'rand2' generating /k/ values.
nrand2 :: (RandomGen g,Random a,Num a) => Int -> a -> g -> ([a],g)
nrand2 k = kvariant k . rand2

-- | @SimpleNumber.rrand@ is 'curry' 'randomR'.
rrand :: (Random n, RandomGen g) => n -> n -> g -> (n,g)
rrand = curry randomR

-- | Variant of 'rrand' generating /k/ values.
nrrand :: (RandomGen g,Random a,Num a) => Int -> a -> a -> g -> ([a],g)
nrrand k = kvariant k .: rrand

-- | @SequenceableCollection.choose@ selects an element at random.
choose :: RandomGen g => [a] -> g -> (a,g)
choose l g =
    let (i,g') = randomR (0,length l - 1) g
    in (l !! i,g')

-- | Variant of 'choose' generating /k/ values.
nchoose :: RandomGen g => Int -> [a] -> g -> ([a],g)
nchoose k = kvariant k . choose

-- | @SimpleNumber.exprand@ generates exponentially distributed random
-- number in the given interval.
exprand :: (Floating n,Random n,RandomGen g) => n -> n -> g -> (n,g)
exprand l r g =
    let (n,g') = rrand 0.0 1.0 g
    in (M.exprange l r n,g')

-- | Variant of 'exprand' generating /k/ values.
nexprand :: (Floating n,Random n,RandomGen g) =>
            Int -> n -> n -> g -> ([n],g)
nexprand k = kvariant k .: exprand

-- | @SimpleNumber.coin@ is 'True' at given probability, which is in
-- range (0,1).
coin :: (RandomGen g, Random a, Ord a, Fractional a) => a -> g -> (Bool,g)
coin n g =
  let (i,g') = randomR (0.0,1.0) g
  in (i < n,g')

-- | Variant of 'coin' generating /k/ values.
--
-- > fst (ncoin 5 0.5 (mkStdGen 0)) == [True,True,False,True,False]
ncoin :: (RandomGen g, Random a, Ord a, Fractional a) => Int -> a -> g -> ([Bool],g)
ncoin k = kvariant k . coin

-- | @List.scramble@ shuffles the elements.
--
-- > fst (scramble [1..5] (mkStdGen 0)) == [1,5,2,3,4]
scramble :: RandomGen g => [t] -> g -> ([t],g)
scramble k g =
    let (_,g') = next g
    in (shuffle' k (length k) g,g')

-- | @SequenceableCollection.wchoose@ selects an element from a list
-- given a list of weights which sum to @1@.
--
-- > kvariant 10 (wchoose "abcd" (C.normalizeSum [8,4,2,1])) (mkStdGen 0)
wchoose :: (RandomGen g,Random a,Ord a,Fractional a) => [b] -> [a] -> g -> (b,g)
wchoose l w g =
  let (i,g') = randomR (0.0,1.0) g
      n = fromMaybe (error "wchoose: windex") (C.windex w i)
  in (l !! n,g')

-- | Variant that applies 'C.normalizeSum' to weights.
--
-- > let r = "dcbbacaadd"
-- > in r == fst (kvariant 10 (wchoose_N "abcd" [8,4,2,1]) (mkStdGen 0))
wchoose_N :: (Fractional a,Ord a,RandomGen g,Random a) => [b] -> [a] -> g -> (b, g)
wchoose_N l w = wchoose l (C.normalizeSum w)

-- | 'kvariant' of 'wchoose_N'.
nwchoose_N :: (Fractional a,Ord a,RandomGen g,Random a) => Int -> [b] -> [a] -> g -> ([b], g)
nwchoose_N n = kvariant n .: wchoose_N
