filter :: (a -> Bool) -> [a] -> [a]
pfilter :: (a -> Bool) -> P a -> P a

Allows values for which the predicate is true.  Aliased to pselect.
See also preject.

> import Sound.SC3.Lang.Pattern.List

> filter (< 3) (P.seq [1,2,3] 3)
> pfilter (< 3) (pseq [1,2,3] 3)

> pselect (< 3) (pseq [1,2,3] 3)
