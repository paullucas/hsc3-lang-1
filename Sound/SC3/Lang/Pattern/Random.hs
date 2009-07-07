module Sound.SC3.Lang.Pattern.Random where

import Control.Monad
import Data.Array
import Data.List
import Sound.SC3.Lang.Pattern.Pattern
import Sound.SC3.Lang.Pattern.Control
import Sound.SC3.Lang.Pattern.List
import qualified System.Random as R

-- Random numbers

pfixR :: Int -> P R.StdGen a -> P R.StdGen a
pfixR = pfix . R.mkStdGen

evalR :: Int -> P R.StdGen a -> [a]
evalR n = pfoldr (R.mkStdGen n) (:) []

prrandf :: (R.Random a, R.RandomGen s) => 
           (a -> a -> a -> a) -> a -> a -> P s a
prrandf f l r = prp (\g -> let (x, g') = R.randomR (l,r) g
                           in (preturn (f l r x), g'))

prrand :: (R.Random a, R.RandomGen s) => 
          a -> a -> P s a
prrand = prrandf (\_ _ x -> x)

prrandexp :: (Floating a, R.Random a, R.RandomGen s) => 
             a -> a -> P s a
prrandexp = prrandf (\l r x -> l * (log (r / l) * x))

pchoosea :: R.RandomGen s => Array Int (P s a) -> P s a
pchoosea r = prp (\g -> let (i, g') = R.randomR (bounds r) g 
                        in (r ! i, g'))

pchoose :: R.RandomGen s => [P s a] -> P s a
pchoose l = pchoosea (listArray (0, length l - 1) l)

prand :: R.RandomGen s => [P s a] -> P s Int -> P s a
prand p = pseq [pchoose p]

pwhite :: (R.RandomGen s, R.Random a) => 
          P s a -> P s a -> P s Int -> P s a
pwhite l r n = prestrict n (join (pzipWith prrand l r))

pexprand :: (Floating a, R.RandomGen s, R.Random a) => 
            P s a -> P s a -> P s Int -> P s a
pexprand l r n = prestrict n (join (pzipWith prrandexp l r))

pxrand :: (Eq a, R.RandomGen s) => [P s a] -> P s Int -> P s a
pxrand p n = ptake n (prsd (pseq [pchoose p] pinf))

pwrand :: R.RandomGen s => [P s a] -> [P s a] -> P s Int -> P s a
pwrand = undefined
