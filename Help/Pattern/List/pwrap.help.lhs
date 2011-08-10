wrap' :: (Num a,Ord a) => (a,a) -> a -> a
wrap :: (Num a,Ord a) => [a] -> a -> a -> [a]
pwrap :: (Ord a, Num a) => P a -> a -> a -> P a

Constrain the range of output values by wrapping.

> import Sound.SC3.Lang.Pattern.List as P

> map (wrap' (200,1000.0)) (geom 200 1.07 26)
> wrap (geom 200 1.07 26) 200 1000.0
> pwrap (pgeom 200 1.07 26) 200 1000.0
