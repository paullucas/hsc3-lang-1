pseries :: (Num a) => a -> a -> Int -> P a

An arithmetric series. 

  start - start value
   step - addition factor
 length - number of values

> let p = pseries 0 2 24
> in evalP p

> let p = pseries 1.0 0.1 24
> in evalP p
