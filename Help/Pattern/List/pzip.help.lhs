pzip :: P a -> P b -> P (a, b)

> import Sound.SC3.Lang.Pattern.List

> ptake 5 (pzip (prepeat 3) (prepeat 4))

Stops on shortest pattern.

> ptake 5 (pzip (pseries 0 1 5) (pseries 0 (-1) 4))
