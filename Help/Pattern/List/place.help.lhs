lace :: [[a]] -> Int -> [a]
place :: [P a] -> Int -> P a

Interlaced embedding of subarrays.

> lace [1,[2,5],[3,6]] 3
> take 8 (lace [1,[2,5],[3,6]] inf)

> place [1,fromList [2,5],fromList [3,6]] 3
> ptake 8 (place [1,fromList [2,5],fromList [3,6]] inf)
