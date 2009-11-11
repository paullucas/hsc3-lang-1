prand :: [P a] -> P Int -> P a

Returns one item from the list at random for each repeat. 

> import Sound.SC3.Lang.Pattern.Step

> let p = prand [1, 2, 3, 4, 5] 6
> in evalP p

> let p = prand [ pseq [1, 2] 1
>               , pseq [3, 4] 1
>               , pseq [5, 6] 1 ] 9
> in evalP p

> import Data.Monoid

> let p = pseq [prand [mempty, pseq [24, 31, 36, 43, 48, 55] 1] 1
>              ,pseq [60, prand [63, 65] 1
>                    ,67, prand [70, 72, 74] 1] (prrand 2 5)
>              ,prand [74, 75, 77, 79, 81] (prrand 3 9)] pinf
> in take 24 (evalP p)
