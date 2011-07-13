choose :: ID n => n -> [a] -> [a]
pchoose :: ID n => n -> P a -> P a

Returns one item from a finite pattern at random for each step.

> import Sound.SC3.Lang.Pattern.List as P

> let p = choose 'x' (P.seq [1,2,3,4,5] 1)
> in take 5 p

> let p = pchoose 'x' (pseq [1,2,3,4,5] 1)
> in ptake 5 p
