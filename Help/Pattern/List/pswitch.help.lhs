switch :: [[a]] -> [Int] -> [a]
pswitch :: [P a] -> P Int -> P a

Select elements from a list of patterns by a pattern of indices.

switch l i = i >>= (l !!)

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

> switch [[1,2,3,1,2,3],[65,76],800] [2,2,0,1]

> pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (fromList [2,2,0,1])

