fmap :: (Functor f) => (a -> b) -> f a -> f b
pcollect :: (a -> b) -> P a -> P b

Patterns are functors.

> import Sound.SC3.Lang.Pattern.List

> pcollect (* 3) (pseq [1, 2, 3] 3)
