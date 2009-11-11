preject :: (a -> Bool) -> P a -> P a
preject f = pfilter (not . f)

Rejects values for which the predicate is true. 

> import Sound.SC3.Lang.Pattern.List

> let p = pseq [1, 2, 3] 3
> in preject (== 1) p
