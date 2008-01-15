pwrap :: (Ord a, Num a) => P a -> P a -> P a -> P a

 x - input
 l - lower bound
 r - upper bound

If x is outside of (l, r) wrap until it lies inside.

> import Sound.SC3.Lang.Pattern

> let { p = pseries 6 2 9
>     ; q = pwrap p 2 10 }
> in pureP q

> let { p = pseries 6 2 9
>     ; q = pwrap p 1 11 }
> in pureP q
