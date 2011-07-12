fmap :: (Functor f) => (a -> b) -> f a -> f b
collect :: (a -> b) -> [a] -> [b]
pcollect :: (a -> b) -> P a -> P b

Patterns are functors.

> import Sound.SC3.Lang.Pattern.List as P

> fmap (* 3) (pseq [1, 2, 3] 3)

> collect (* 3) (P.seq [1, 2, 3] 3)

> pcollect (* 3) (pseq [1, 2, 3] 3)
