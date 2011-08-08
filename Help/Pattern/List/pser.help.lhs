lser :: [[a]] -> Int -> [a]
pser :: [P a] -> Int -> P a

pser is like pseq, however the repeats variable
gives the number of elements in the sequence,
not the number of cycles of the pattern.

> import Sound.SC3.Lang.Pattern.List

> lser [1,2,3] 5
> pser [1,2,3] 5

> lseq [1,lseq [100,200] 2,3] 2
> pser [1,pser [100,200] 3,3] 9

> lser [1,2,3] 5 * 3
> pser [1,2,3] 5 * 3
