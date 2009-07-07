pappend :: P a -> P a -> P a

Sequence two patterns.  This is the mappend instance of Monoid.

> import Data.Monoid
> import Sound.SC3.Lang.Pattern

> let { p = pseq [1, 2] 1
>     ; q = pseq [2, 3] 1 }
> in evalP (p `mappend` q)

> evalP (pempty `mappend` pempty)
