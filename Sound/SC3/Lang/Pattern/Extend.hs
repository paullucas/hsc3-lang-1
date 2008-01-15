module Sound.SC3.Lang.Pattern.Extend where

import Control.Applicative
import Sound.SC3.Lang.Pattern.Pattern

pzipWithL :: (a -> b -> c) -> P a -> P b -> P c
pzipWithL f p = pappl (pure f <*> p)

pzipWith3L :: (a -> b -> c -> d) -> P a -> P b -> P c -> P d
pzipWith3L f p q = pappl ((pure f <*> p) `pappl` q)

pzipL :: P a -> P b -> P (a,b)
pzipL = pzipWithL (,)

(+.) :: Num a => P a -> P a -> P a
(+.) = pzipWithL (+)

(*.) :: Num a => P a -> P a -> P a
(*.) = pzipWithL (*)

(/.) :: Fractional a => P a -> P a -> P a
(/.) = pzipWithL (/)

(-.) :: Num a => P a -> P a -> P a
(-.) = pzipWithL (-)
