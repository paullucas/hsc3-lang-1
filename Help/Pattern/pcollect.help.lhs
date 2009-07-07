pcollect :: (a -> b) -> P a -> P b
pcollect = fmap

Patterns are functors.

> import Sound.SC3.Lang.Pattern

> let p = pcollect (* 3) (pseq [1, 2, 3] 3)
> in evalP p
