pseq :: [[a]] -> Int -> [a]

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> import Sound.SC3.Lang.Pattern.List as P

> pseq [1,2,3] 2
> pseq [1,pn 2 2,3] 2
> pseq [2,3,4] 12

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C

> pseq (C.rotate 3 [1,2,3,4]) 3

There is an 'infinite' value for the repeats variable.

> ptake 9 (pseq [1,2,3] inf)
