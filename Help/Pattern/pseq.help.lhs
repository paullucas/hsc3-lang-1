pseq :: [P a] -> P Int -> P a
pseq_ :: [P a] -> Int -> P a

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list. 

> import Sound.SC3.Lang.Pattern

> let a = pseq [1, 2, 3] 2
> in evalP 0 a

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> import Sound.SC3.Lang.Collection

> let p = pseq (rotate 3 [1, 2, 3, 4]) 3
> in evalP 0 p

Because the repeat counter is a pattern one can have
a random number of repeats.

> let p = pseq [1, 2] (prrand 1 9)
> in evalP 7 p

For the same reason the pattern is not static when 
re-examined.

> let p = pseq [ 0, pseq [1] (prrand 1 3), 2] 5
> in take 24 (evalP 94 p)

Further, if the repeat pattern is not singular,
the sequence will repeat until the pattern is exhausted.

> let { p = pseq [1] 3
>     ; q = pseq [1] p }
> in evalP 0 q

If one specifies the value pinf for the repeats variable, 
then it will repeat indefinitely.

> let p = pseq [1, 2, 3] pinf
> in take 9 (evalP 0 p)

There is a variant with a true integer repeat count.

> let p = pseq_ [1, 2, 3] 5
> in evalP 0 p
