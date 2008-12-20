module Sound.SC3.Lang.Pattern.Extend where

import Sound.SC3.Lang.Pattern.Pattern

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
