fbool :: (Functor f, Ord a, Num a) => f a -> f Bool
pbool :: (Ord a, Num a) => P a -> P Bool

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> pbool (fromList [1,0,1,0,0,0,1,1])
