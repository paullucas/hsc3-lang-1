module Sound.SC3.Lang.Pattern.Random where

import Control.Monad
import Data.Array
import Data.List
import Sound.SC3.Lang.Pattern.Pattern
import Sound.SC3.Lang.Pattern.Control
import Sound.SC3.Lang.Pattern.List
import qualified System.Random as R

-- Random numbers

prrandf :: (R.Random a) => (a -> a -> a -> a) -> a -> a -> P a
prrandf f l r = prp (\g -> let (x, g') = R.randomR (l,r) g
                           in (return (f l r x), g'))

prrand :: (R.Random a) => a -> a -> P a
prrand = prrandf (\_ _ x -> x)

prrandexp :: (Floating a, R.Random a) => a -> a -> P a
prrandexp = prrandf (\l r x -> l * (log (r / l) * x))

pchoosea :: Array Int (P a) -> P a
pchoosea r = prp (\g -> let (i, g') = R.randomR (bounds r) g 
                        in (r ! i, g'))

pchoose :: [P a] -> P a
pchoose l = pchoosea (listArray (0, length l - 1) l)

prand :: [P a] -> P Int -> P a
prand p = pseq [pchoose p]

pwhite :: (R.Random a) => P a -> P a -> P Int -> P a
pwhite l r n = prestrict n (join (pzipWith prrand l r))

pexprand :: (Floating a, R.Random a) => P a -> P a -> P Int -> P a
pexprand l r n = prestrict n (join (pzipWith prrandexp l r))

pxrand :: (Eq a) => [P a] -> P Int -> P a
pxrand p n = ptake n (prsd (pseq [pchoose p] pinf))

pwrand :: [P a] -> [P a] -> P Int -> P a
pwrand = undefined
