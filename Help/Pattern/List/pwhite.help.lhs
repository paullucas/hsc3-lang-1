pwhite :: (Random a) => String -> P a -> P a -> P a

Uniform linear distribution in given range.

> import Sound.SC3.Lang.Pattern.List

> phead (pwhite "x" 0.0 1.0)

> ptake 5 (pwhite "x" 0.0 1.0)

It is important to note that this structure is not actually
indeterminate, so that the below is zero.

> let p = ptake 12 (pwhite "x" 0.0 1.0)
> in p - p

And likewise the below is a list of two equal elements.

> let { p = pwhite "x" 1 10
>     ; q = ptake 1 p }
> in pseq [q, q] 1

The below is alternately lower and higher noise.

> let { l = pseq [0.0, 10.0] 1
>     ; r = pseq [1.0, 11.0] 1
>     ; p = pwhite "x" l r }
> in ptake 12 p

Or equivalently,

> let { b = pseq [return (0.0, 1.0)
>                ,return (10.0, 11.0)] 1
>     ; p = pwhite "x" (fmap fst b) (fmap snd b) }
> in ptake 12 p
