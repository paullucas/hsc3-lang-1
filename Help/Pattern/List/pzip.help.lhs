zip :: [a] -> [b] -> [(a,b)]
pzip :: P a -> P b -> P (a,b)

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (zip (repeat 3) (repeat 4))
> ptake 5 (pzip (prepeat 3) (prepeat 4))

Note that zip is truncating wheras pzip extending.

> zip [1 .. 6] [-1,-2,-3]
> pzip (fromList [1 .. 6]) (fromList [-1,-2,-3])
