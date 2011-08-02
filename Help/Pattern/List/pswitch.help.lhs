pswitch :: [[a]] -> [Int] -> [a]

Select elements from a list of patterns by a pattern of indices.

pswitch l i = i >>= (l !!)

> import Sound.SC3.Lang.Pattern.List as P

> let {a = pseq [1,2,3] 2
>     ;b = pseq [65,76] 1
>     ;c = pswitch [a,b,800] (pseq [2,2,0,1] inf)}
> in ptake 24 c
