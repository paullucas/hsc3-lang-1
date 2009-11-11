clutch :: [a] -> [Bool] -> [a]
pclutch :: P a -> P Bool -> P a

 i - input
 c - clutch

Sample and hold a pattern.  For true values in the control pattern,
step the value pattern, else hold the previous value.

> import Sound.SC3.Lang.Pattern.List

> let { p = pseq [1, 2, 3, 4, 5] 3
>     ; q = pbool (pseq [1, 0, 1, 0, 0, 0, 1, 1] 1) }
> in pclutch p q

Note the initialization behavior, nothing
is generated until the first true value.

> let { p = pseq [1, 2, 3, 4, 5] 3
>     ; q = pbool (pseq [0, 0, 0, 1, 0, 0, 1, 0, 1] 1) }
> in pclutch p q
