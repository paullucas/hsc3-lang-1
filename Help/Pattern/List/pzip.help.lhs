zip :: [a] -> [b] -> [(a, b)]
pzip :: P a -> P b -> P (a, b)

> import Sound.SC3.Lang.Pattern.List

> take 5 (zip (repeat 3) (repeat 4))
> ptake 5 (pzip (prepeat 3) (prepeat 4))

Stops on shortest pattern.

> zip [0 ..] [0,-1 .. -3]
> zip (series 0 1 5) (series 0 (-1) 4)
> pzip (pseries 0 1 5) (pseries 0 (-1) 4)

