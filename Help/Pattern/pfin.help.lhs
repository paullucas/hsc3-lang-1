pfin :: P Int -> P a -> P a
pfin_ :: Int -> P a -> P a
ptake :: P Int -> P a -> P a
ptake_ :: P Int -> P a -> P a

  n - number of elements to take
  x - value pattern

Take only the first n elements of the pattern 
into the stream.

> import Sound.SC3.Lang.Pattern

> let p = pseq [1, 2, 3] pinf
> in pureP (pfin 5 p)

There is a variant where the count not a pattern.

> let p = pseq [1, 2, 3] 1
> in pureP (pfin_ 5 p)

Note that pfin does not extend the input pattern,
unlike pser.

> let p = pseq [1, 2, 3] 1
> in pureP (pser [p] 5)
