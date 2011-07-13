interleave :: [a] -> [a] -> [a]
pinterleave :: P a -> P a -> P a

Interleave elements from two patterns.  If one pattern ends the other
pattern continues until it also ends.

> import Sound.SC3.Lang.Pattern.List as P

> let {p = P.seq [1,2,3] 3
>     ;q = P.seq [4,5,6,7] 2}
> in interleave p q

> let {p = pseq [1,2,3] 3
>     ;q = pseq [4,5,6,7] 2}
> in pinterleave p q

> take 10 (interleave (cycle 1) (cycle 2))
> ptake 10 (pinterleave (pcycle 1) (pcycle 2))

> take 10 (interleave (white 'x' 1 9) (series 10 1 5))
> ptake 10 (pinterleave (pwhite 'x' 1 9) (pseries 10 1 5))
