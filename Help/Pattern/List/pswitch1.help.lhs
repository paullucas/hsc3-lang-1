switch1 :: [[a]] -> [Int] -> [a]
pswitch1 :: [P a] -> P Int -> P a

  list - patterns to index
 which - index

The pattern of indices is used select which pattern
to retrieve the next value from.  Only one value
is selected from each the pattern.

This is in comparison to pswitch,which embeds the
pattern in its entirety.  pswitch1 switches every value.

> import Sound.SC3.Lang.Pattern.List as P

> let {p = P.seq [1,2,3] inf
>     ;q = P.seq [65,76] inf
>     ;r = switch1 [p,q,nof 800 3] (P.seq [2,0,1] inf)}
> in take 24 r

> let {p = pseq [1,2,3] pinf
>     ;q = pseq [65,76] pinf
>     ;r = pswitch1 [p,q,pn 800 3] (pseq [2,0,1] pinf)}
> in ptake 24 r
