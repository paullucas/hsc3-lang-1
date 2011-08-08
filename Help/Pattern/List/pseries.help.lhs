series :: (Num a) => a -> a -> Int -> [a]
pseries :: (Num a) => a -> a -> Int -> P a

An arithmetric series.

  start - start value
   step - addition factor
 length - number of values

> import Sound.SC3.Lang.Pattern.List as P

> series 0 2 12
> series 1.0 0.2 6

> pseries 0 2 12
> pseries 1.0 0.2 6
