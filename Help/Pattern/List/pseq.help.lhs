pseq :: [[a]] -> Int -> [a]

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> import Sound.SC3.Lang.Pattern.List as P

> pseq [[1],[2],[3]] 2
> pseq [1,2,3] 2

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> import Sound.SC3.Lang.Collection.SequenceableCollection

> pseq (rotate 3 [1,2,3,4]) 3

The repeat counter is not a pattern.

> pseq [1,2] (rrand 'u' 1 9)

For the same reason the pattern is static when re-examined.

> let p = pseq [0,pseq [1] (rrand 'u' 1 3),2] 5 in take 24 p

There is an 'infinite' value for the repeats variable.

> take 9 (pseq [1,2,3] inf)
