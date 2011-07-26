fmap :: (Functor f) => (a -> b) -> f a -> f b

Patterns are functors.

> import Sound.SC3.Lang.Pattern.List as P

> fmap (* 3) [1, 2, 3]
