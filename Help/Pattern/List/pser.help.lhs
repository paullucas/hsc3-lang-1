pser :: [P a] -> P Int -> P a

pser is like pseq, however the repeats variable 
gives the number of elements in the sequence,
not the number of cycles of the pattern.

> import Sound.SC3.Lang.Pattern.List

> pser [1, 2, 3] 5

> pser [1, pser [100, 200] 3, 3] 9

> pser [1, 2, 3] 5 *. 3
