pinterleave :: [a] -> [a] -> [a]

Interleave elements from two patterns.  If one pattern ends the other
pattern continues until it also ends.

> import Sound.SC3.Lang.Pattern.List as P

> let {p = pseq [1,2,3] 3
>     ;q = pseq [4,5,6,7] 2}
> in pinterleave p q

> ptake 10 (pinterleave (pcycle 1) (pcycle 2))

> ptake 10 (pinterleave (pwhite 'a' 1 9) (pseries 10 1 5))
