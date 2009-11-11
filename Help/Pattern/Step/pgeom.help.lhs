pgeom :: (Num a) => a -> a -> Int -> P a

Geometric series pattern.

  start - start value
   grow - multiplication factor
 length - number of values produced

> import Sound.SC3.Lang.Pattern.Step

> let p = pgeom 1 2 12
> in evalP p

Real numbers work as well.

> let p = pgeom 1.0 1.1 6
> in evalP p
