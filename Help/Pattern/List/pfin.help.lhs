take :: Int -> [a] -> [a]
ptake :: P Int -> P a -> P a

  n - number of elements to take
  x - value pattern

Take only the first n elements of the pattern 
into the stream.  pfin = ptake.

> import Sound.SC3.Lang.Pattern.List

> let p = pseq [1, 2, 3] pinf
> in pfin 5 p

Note that pfin does not extend the input pattern,
unlike pser.

> let p = pseq [1, 2, 3] 1
> in (pfin 5 p, pser [p] 5)
