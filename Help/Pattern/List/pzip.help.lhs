zip :: [a] -> [b] -> [(a, b)]
pzip :: P a -> P b -> P (a, b)

> import Sound.SC3.Lang.Pattern.List

> ptake 5 (pzip (prepeat 3) (prepeat 4))

Stops on shortest pattern.

> pzip (pseries 0 1 5) (pseries 0 (-1) 4)

> zip [1..] [3,2,1] == [(1,3),(2,2),(3,1)]
