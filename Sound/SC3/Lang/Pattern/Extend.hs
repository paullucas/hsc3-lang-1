module Sound.SC3.Lang.Pattern.Extend where

import Sound.SC3.Lang.Pattern.Pattern

pzipWith_c :: (a -> b -> c) -> P s a -> P s b -> P s c
pzipWith_c f p = pzipWith f p . pcycle

(+.) :: Num a => P s a -> P s a -> P s a
(+.) = pzipWith_c (+)

(*.) :: Num a => P s a -> P s a -> P s a
(*.) = pzipWith_c (*)

(/.) :: Fractional a => P s a -> P s a -> P s a
(/.) = pzipWith_c (/)

(-.) :: Num a => P s a -> P s a -> P s a
(-.) = pzipWith_c (-)
