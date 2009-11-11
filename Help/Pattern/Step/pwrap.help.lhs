pwrap :: (Ord a, Num a) => P a -> P a -> P a -> P a

 x - input
 l - lower bound *cycle*
 r - upper bound *cycle*

If x is outside of (l, r) wrap until it lies inside.

> import Sound.SC3.Lang.Pattern.Step

> let { p = pseries 6 2 9
>     ; q = pwrap p 2 10 }
> in evalP q

> let { p = pseries 6 2 9
>     ; q = pwrap p 1 11 }
> in evalP q
