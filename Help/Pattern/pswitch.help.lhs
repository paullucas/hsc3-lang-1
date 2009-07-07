pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

Select elements from a list of patterns by a pattern of indices.

> import Sound.SC3.Lang.Pattern

> let { a = pseq [1, 2, 3] 2
>     ; b = pseq [65, 76] 1
>     ; c = pswitch [a, b, 800] (pseq [2, 2, 0, 1] pinf) }
> in take 24 (evalP c)
