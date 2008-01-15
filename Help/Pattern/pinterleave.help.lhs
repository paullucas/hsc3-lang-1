pinterleave :: P a -> P a -> P a

Interleave elements from two patterns.

> import Sound.SC3.Lang.Pattern

> let p = pinterleave (pwhite 1 9 5) (pseries 10 1 10)
> in evalP 1317 p

There is a variant without implicit extension.

> let p = pinterleave' (pwhite 1 9 5) (pseries 10 1 10)
> in evalP 1317 p
