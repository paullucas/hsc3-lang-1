seq :: [[a]] -> [Int] -> [a]
pseq :: [P a] -> P Int -> P a

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> import Sound.SC3.Lang.Pattern.List as P

> P.seq [[1],[2],[3]] [2]
> P.seq [1,2,3] 2

> pseq [1,2,3] 2

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> import Sound.SC3.Lang.Collection.SequenceableCollection

> P.seq (rotate 3 [1,2,3,4]) 3
> pseq (rotate 3 [1,2,3,4]) 3

Because the repeat counter is a pattern one can have
a random number of repeats.

> P.seq [1,2] (white 'u' 1 9)
> pseq [1,2] (pwhite 'u' 1 9)

For the same reason the pattern is static when re-examined.

> let p = pseq [0,pseq [1] (pwhite 'u' 1 3),2] 5
> in ptake 24 p

Only the first element of the repeat pattern is consulted.

> let p = P.seq [1,2] 1 in P.seq [1] p
> let p = pseq [1,2] 1 in pseq [1] p

If one specifies the value pinf for the repeats variable,
then it will repeat indefinitely.

> take 9 (P.seq [1,2,3] inf)
> ptake 9 (pseq [1,2,3] pinf)
