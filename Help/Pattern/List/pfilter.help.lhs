pfilter :: (a -> Bool) -> P a -> P a

Allows values for which the predicate is true. 

> import Sound.SC3.Lang.Pattern.List

> let p = pseq [1, 2, 3] 3
> in pfilter (< 3) p
