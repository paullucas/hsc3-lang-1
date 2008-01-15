preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

Rejects values for which the predicate is true. 

> import Sound.SC3.Lang.Pattern

> let { p = pseq [1, 2, 3] 3
>     ; q = preject (== 1) p }
> in pureP q
