pappend :: P a -> P a -> P a

Sequence two patterns.  This is the mappend instance of Monoid.

> import Sound.SC3.Lang.Pattern.List

> let { p = pseq [1, 2] 1
>     ; q = pseq [2, 3] 1 }
> in p `pappend` q

> pnull (pempty `pappend` pempty)

> ptake 5 (prepeat 3 `pappend` prepeat 4)
