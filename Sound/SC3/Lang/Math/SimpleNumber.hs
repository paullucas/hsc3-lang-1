module Sound.SC3.Lang.Math.SimpleNumber where

import System.Random {- random -}

rrand' :: (Random n, Enum a) => a -> (n,n) -> n
rrand' j k =
    let g = mkStdGen (fromEnum j)
    in fst (randomR k g)

rrand :: (Random n) => n -> n -> IO n
rrand = curry randomRIO

rand :: (Random n,Num n) => n -> IO n
rand n = randomRIO (0,n)

rand2 :: (Random n,Num n) => n -> IO n
rand2 n = randomRIO (-n,n)

coin :: (Random n,Num n,Ord n) => n -> IO Bool
coin n = do
  i <- randomRIO (0,1)
  return (i < n)

{-
import Control.Monad
replicateM 12 (coin 0.5)
-}
