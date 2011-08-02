pwhite :: (Enum e,Random a) => e -> P a -> P a -> P a

Uniform linear distribution in given range.

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> ptake 5 (pwhite 'x' 0.0 1.0)

It is important to note that this structure is not actually
indeterminate, so that the below is zero.

> let p = ptake 12 (pwhite 'x' 0.0 1.0) in p - p

The below is alternately lower and higher noise.

> let p = pwhite 'x' (pseq [0.0,10.0] inf) (pseq [1.0,11.0] inf)
> in ptake 12 p
