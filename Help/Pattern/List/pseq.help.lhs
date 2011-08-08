pseq :: [[a]] -> Int -> [a]

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> import Sound.SC3.Lang.Pattern.List as P

> lseq [[1],[2],[3]] 2
> lseq [1,2,3] 2
> pseq [return 1,return 2,return 3] 2
> pseq [1,2,3] 2

> lseq [1,ln 2 2,3] 2
> pseq [1,pn 2 2,3] 2

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C

> lseq (C.rotate 3 [1,2,3,4]) 3
> pseq (C.rotate 3 [1,2,3,4]) 3

There is an 'infinite' value for the repeats variable.

> ptake 9 (pdrop 1000000 (pseq [1,2,3] inf))
