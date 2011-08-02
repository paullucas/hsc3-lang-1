fmap :: (Functor f) => (a -> b) -> f a -> f b
pmap :: (a -> b) -> P a -> P b

Patterns are functors.

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> pmap (* 3) (fromList [1,2,3])
