lace :: [[a]] -> Int -> [a]
place :: [P a] -> Int -> P a

Interlaced embedding of subarrays.

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

> lace [1,[2,5],[3,6]] 3
> lace [1,[2,5],[3,6..]] 5

> place [1,fromList [2,5],fromList [3,6]] 3
> place [1,fromList [2,5],pseries 3 3 inf] 5
