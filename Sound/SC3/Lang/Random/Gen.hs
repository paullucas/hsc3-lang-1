-- | 'RandomGen' based @sclang@ random number functions.
module Sound.SC3.Lang.Random.Gen where

import Data.Maybe
import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Math as M
import System.Random {- random -}
import System.Random.Shuffle {- random-shuffle -}

-- | @SimpleNumber.rand@ is 'randomR' in (0,/n/).
rand :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand n = randomR (0,n)

-- | Construct variant of /f/ generating /k/ values.
kvariant :: Int -> (g->(a,g)) -> g->([a],g)
kvariant k f =
    let go x i g = case i of
                     0 -> (x,g)
                     _ -> let (y,g') = f g
                          in go (y:x) (i - 1) g'
    in go [] k

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
nrrand k l = kvariant k . rrand l

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
    in (M.exprandrng l r n,g')

-- | Variant of 'exprand' generating /k/ values.
nexprand :: (Floating n,Random n,RandomGen g) =>
            Int -> n -> n -> g -> ([n],g)
nexprand k l = kvariant k . exprand l

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
wchoose :: (RandomGen g,Random a,Ord a,Fractional a) => [b] -> [a] -> g -> (b,g)
wchoose l w g =
  let (i,g') = randomR (0.0,1.0) g
      n = fromMaybe (error "wchoose: windex") (C.windex w i)
  in (l !! n,g')
