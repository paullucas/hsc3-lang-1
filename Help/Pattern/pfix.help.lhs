pfix :: Int -> P a -> P a

> import Sound.SC3.Lang.Pattern

The psuedo-random nodes are actually indeterminate.
To fix the value of these nodes use pfix.

> let p = pwhite 0.0 1.0 12
> in evalP (p - p)

> let p = pfix 0 (pwhite 0.0 1.0 12)
> in evalP (p - p)

The innermost pfix is binding.

> let p = pwhite 0.0 1.0 12
> in evalP (pzip (pfix 1 (pfix 0 p) - pfix 0 p)
>                (pfix 0 (pfix 1 p) - pfix 0 p))
