pn :: P a -> P Int -> P a
preplicate :: P Int -> P a -> P a

Repeats the enclosed pattern a number of times.

> import Sound.SC3.Lang.Pattern

> let p = pn (pseq [1, 2, 3] 1) 4
> in pureP p

There is a variant with the arguments
reversed.

> let p = preplicate 4 (pseq [1, 2, 3] 1)
> in pureP p

