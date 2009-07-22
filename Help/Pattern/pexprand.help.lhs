pexprand :: (Floating a, Random a) => P a -> P a -> P Int -> P a

Exponential distribution distribution in given range.

> import Sound.SC3.Lang.Pattern

> let p = pexprand 0.01 0.99 12
> in evalP p

> let { l = pseq [1, 11] 1
>     ; r = pseq [2, 12] 1
>     ; p = pexprand l r 12 }
> in evalP p
