reject :: (a -> Bool) -> [a] -> [a]
preject :: (a -> Bool) -> P a -> P a

Rejects values for which the predicate is true.
reject f = filter (not . f)

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

> reject (== 1) (P.seq [1,2,3] 3)
> preject (== 1) (pseq [1,2,3] 3)

> filter (not . (== 1)) (P.seq [1,2,3] 3)
> pfilter (not . (== 1)) (pseq [1,2,3] 3)
