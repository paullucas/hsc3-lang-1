geom :: (Num a) => a -> a -> Int -> [a]
pgeom :: (Num a) => a -> a -> Int -> P a

Geometric series pattern.

  start - start value
   grow - multiplication factor
 length - number of values produced

> import Sound.SC3.Lang.Pattern.List as P

> geom 1 2 12
> pgeom 1 2 12

Real numbers work as well.

> geom 1.0 1.1 6
> pgeom 1.0 1.1 6
