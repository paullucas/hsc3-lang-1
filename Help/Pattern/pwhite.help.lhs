pwhite :: (Random a) => P a -> P a -> P Int -> P a

Uniform linear distribution in given range.

> import Sound.SC3.Lang.Pattern

> let p = pwhite 0.0 1.0 12
> in evalP p

It is important to note that this structure is
actually indeterminate, so that the below is
non-zero.

> let p = pwhite 0.0 1.0 12
> in evalP (p - p)

> let { l = pseq [0.0, 10.0] 1
>     ; r = pseq [1.0, 11.0] 1
>     ; p = pwhite l r 12 }
> in evalP p

Or equivalently,

> let { b = pseq [return (0.0, 1.0)
>                ,return (10.0, 11.0)] 1
>     ; p = pwhite (fmap fst b) (fmap snd b) 12 }
> in evalP p
