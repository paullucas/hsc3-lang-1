fmap :: (Functor f) => (a -> b) -> f a -> f b
pmap :: (a -> b) -> P a -> P b

Patterns are functors.

> import Sound.SC3.Lang.Pattern.List

> fmap (* 3) [1,2,3]
> fmap (* 3) (fromList [1,2,3])
