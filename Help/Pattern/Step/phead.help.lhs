phead :: P a -> P a

Retain only the first element of a pattern.

> import Data.Monoid
> import Sound.SC3.Lang.Pattern.Step

> let p = pseq [1, 2, 3] 1
> in evalP (phead p)

> let p = pseq [1, 2, 3] 1
> in evalP (phead p `mappend` ptail p)

> evalP (phead mempty)
