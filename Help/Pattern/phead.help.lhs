phead :: P a -> P a

Retain only the first element of a pattern.

> import Data.Monoid
> import Sound.SC3.Lang.Pattern

> let p = pseq [1, 2, 3] 1
> in evalP 0 (phead p)

> let p = pseq [1, 2, 3] 1
> in evalP 0 (phead p `mappend` ptail p)

> evalP 0 (phead pempty)