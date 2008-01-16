module Sound.SC3.Lang.Pattern.Random where

import Data.Array
import Data.List
import Sound.SC3.Lang.Pattern.Pattern
import Sound.SC3.Lang.Pattern.Control
import Sound.SC3.Lang.Pattern.List
import System.Random

-- Random numbers

prrandf :: (Random a) => (a -> a -> a -> a) -> a -> a -> P a
prrandf f l r = prvalue (\g -> let (x, g') = randomR (l,r) g
                               in (preturn (f l r x), g'))

prrand :: (Random a) => a -> a -> P a
prrand = prrandf (\_ _ x -> x)

prrandexp :: (Floating a, Random a) => a -> a -> P a
prrandexp = prrandf (\ l r x -> l * (log (r / l) * x))

pchoosea :: Array Int (P a) -> P a
pchoosea r = prvalue (\g -> let (i, g') = randomR (bounds r) g 
                            in (r ! i, g'))

pchoose :: [P a] -> P a
pchoose l = pchoosea (listArray (0, length l - 1) l)

prand :: [P a] -> P Int -> P a
prand p = pseq [pchoose p]

pwhite :: (Random a) => P a -> P a -> P Int -> P a
pwhite l r n = prestrict n (pzipWith prrand l r >>= id)

pexprand :: (Floating a, Random a) => P a -> P a -> P Int -> P a
pexprand l r n = prestrict n (pzipWith prrandexp l r >>= id)

pxrand :: (Eq a) => [P a] -> P Int -> P a
pxrand p n = ptake n (prsd (pseq [pchoose p] pinf))

pwrand :: [P a] -> [P a] -> P Int -> P a
pwrand = undefined
