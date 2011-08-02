pgeom :: (Num a) => a -> a -> Int -> [a]

Geometric series pattern.

  start - start value
   grow - multiplication factor
 length - number of values produced

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> pgeom 1 2 12

Real numbers work as well.

> pgeom 1.0 1.1 6
