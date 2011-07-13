switch :: [[a]] -> [Int] -> [a]
pswitch :: [P a] -> P Int -> P a

Select elements from a list of patterns by a pattern of indices.

switch l i = i >>= (l !!)

> import Sound.SC3.Lang.Pattern.List as P

> let {a = P.seq [1,2,3] 2
>     ;b = P.seq [65,76] 1
>     ;c = switch [a,b,800] (P.seq [2,2,0,1] inf)}
> in take 24 c

> let {a = pseq [1,2,3] 2
>     ;b = pseq [65,76] 1
>     ;c = pswitch [a,b,800] (pseq [2,2,0,1] pinf)}
> in ptake 24 c
