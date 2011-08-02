zip :: [a] -> [b] -> [(a, b)]
pzip :: P a -> P b -> P (a, b)

> ptake 5 (pzip (prepeat 3) (prepeat 4))

Stops on shortest pattern.

> pzip (fromList [0 ..]) (fromList [0,-1 .. -3])
