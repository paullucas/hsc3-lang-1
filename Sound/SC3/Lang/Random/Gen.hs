module Sound.SC3.Lang.Random.Gen where

import Data.List
import Data.Maybe
import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Math as M
import System.Random {- random -}
import System.Random.Shuffle {- random-shuffle -}

exprand :: (Floating n,Random n,RandomGen g) => n -> n -> g -> (n,g)
exprand l r g =
    let (n,g') = rrand 0.0 1.0 g
    in (M.exprandrng l r n,g')

rrand :: (Random n, RandomGen g) => n -> n -> g -> (n,g)
rrand = curry randomR

nrrand :: (RandomGen g,Random a,Num a) => Int -> a -> a -> g -> ([a],g)
nrrand n l r =
    let go x 0 g = (x,g)
        go x k g =
            let (y,g') = randomR (l,r) g
            in go (y:x) (k - 1) g'
    in go [] n

rand :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand n = randomR (0,n)

rand2 :: (RandomGen g,Random n,Num n) => n -> g -> (n,g)
rand2 n = randomR (-n,n)

nrand2 :: (RandomGen g,Random a,Num a) => Int -> a -> g -> ([a],g)
nrand2 n = nrrand n 0

coin :: (RandomGen g, Random a, Ord a, Fractional a) => a -> g -> (Bool,g)
coin n g =
  let (i,g') = randomR (0.0,1.0) g
  in (i < n,g')

scramble :: RandomGen g => [t] -> g -> ([t],g)
scramble k g =
    let (_,g') = next g
    in (shuffle' k (length k) g,g')

windex :: (Ord a,Num a) => [a] -> a -> Maybe Int
windex w n = findIndex (n <) (C.integrate w)

wchoose :: (RandomGen g,Random a,Ord a,Fractional a) => [b] -> [a] -> g -> (b,g)
wchoose l w g =
  let (i,g') = randomR (0.0,1.0) g
      n = fromMaybe (error "wchoose: windex") (windex w i)
  in (l !! n,g')

choose :: RandomGen g => [a] -> g -> (a,g)
choose l g =
    let (i,g') = randomR (0,length l - 1) g
    in (l !! i,g')
