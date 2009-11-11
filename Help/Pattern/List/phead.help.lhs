phead :: P a -> Maybe a

Inspect the first element of a pattern.

> import Sound.SC3.Lang.Pattern.List

> phead (pseq [1, 2, 3] 1)

> let p = pseq [1, 2, 3] 1
> in maybe pempty (\x -> x `pcons` ptail p) (phead p)

> phead pempty
