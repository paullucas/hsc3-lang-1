pchoose :: String -> P a -> P a
prand :: String -> [P a] -> P Int -> P a

Returns one item from a finite pattern at random for each step. 

> import Sound.SC3.Lang.List

> let p = pchoose "x" (pseq [1, 2, 3, 4, 5] 1)
> in ptake 5 p

> prand "x" [ pseq [1, 2] 1
>           , pseq [3, 4] 1
>           , pseq [5, 6] 1 ] 10

The below cannot be written as intended with the list
based pattern library.  This is precisely because the
noise patterns are values, not processes with a state
threaded non-locally. 

> let p = pseq [prand "a" [pempty, pseq [24, 31, 36, 43, 48, 55] 1] 1
>              ,pseq [60, prand "b" [63, 65] 1
>                    ,67, prand "c" [70, 72, 74] 1] (pwhite "c" 2 5)
>              ,prand "d" [74, 75, 77, 79, 81] (pwhite "e" 3 9)] pinf
> in ptake 24 p

