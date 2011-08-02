pseries :: (Num a) => a -> a -> Int -> [a]

An arithmetric series.

  start - start value
   step - addition factor
 length - number of values

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> pseries 0 2 24
> pseries 1.0 0.1 24
