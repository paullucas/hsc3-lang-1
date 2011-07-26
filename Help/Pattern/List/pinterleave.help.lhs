pinterleave :: [a] -> [a] -> [a]

Interleave elements from two patterns.  If one pattern ends the other
pattern continues until it also ends.

> import Sound.SC3.Lang.Pattern.List as P

> let {p = pseq [1,2,3] 3
>     ;q = pseq [4,5,6,7] 2}
> in pinterleave p q

> take 10 (pinterleave (cycle 1) (cycle 2))

> take 10 (pinterleave (pwhite 'x' 1 9) (pseries 10 1 5))
