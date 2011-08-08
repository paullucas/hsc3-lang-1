switch1 :: [[a]] -> [Int] -> [a]
pswitch1 :: [P a] -> P Int -> P a

  list - patterns to index
 which - index

The pattern of indices is used select which pattern
to retrieve the next value from.  Only one value
is selected from each the pattern.

This is in comparison to pswitch,which embeds the
pattern in its entirety.  pswitch1 switches every value.

> import Sound.SC3.Lang.Pattern.List

> switch1 [[1,2,3],[65,76],cycle 8] (cycle [2,2,0,1])
> pswitch1 [fromList [1,2,3],fromList [65,76],pcycle 8] (pseq [2,2,0,1] inf)

> let {p = cycle [1,2,3]
>     ;q = cycle [65,76]}
> in take 28 (switch1 [p,q,cycle 8] (cycle [2,2,0,1]))

> let {p = pseq [1,2,3] inf
>     ;q = pseq [65,76] inf}
> in ptake 28 (pswitch1 [p,q,pn 8 inf] (pseq [2,2,0,1] inf))

