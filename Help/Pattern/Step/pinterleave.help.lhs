pinterleave :: P a -> P a -> P a

Interleave elements from two patterns.

> import Sound.SC3.Lang.Pattern.Step

> let { p = pseq [1, 2, 3] 3
>     ; q = pseq [4, 5, 6, 7] 2 }
> in evalP (pinterleave p q)

> let p = pinterleave (pwhite 1 9 5) (pseries 10 1 10)
> in evalP p
