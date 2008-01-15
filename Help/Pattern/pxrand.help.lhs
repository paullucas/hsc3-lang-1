pxrand :: (Eq a) => [P a] -> P Int -> P a

Like prand, returns one item from the list at random for each 
step, but pxrand never repeats the same element twice in a row. 

> let p = pxrand [1,2,3] 10
> in evalP 0 p
