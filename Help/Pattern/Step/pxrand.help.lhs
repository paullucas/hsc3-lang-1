pxrand :: (Eq a) => [P a] -> P Int -> P a

Like prand, returns one item from the list at random for each 
step, but pxrand never repeats the same element twice in a row. 

> import Sound.SC3.Lang.Pattern.Step

> let p = pxrand [1,2,3] 10
> in evalP p
