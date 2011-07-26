(++) :: [a] -> [a] -> [a]

Sequence two patterns.  This is the mappend instance of Monoid.

> import Sound.SC3.Lang.Pattern.List

> [1,2] ++ [2,3]

> null ([] ++ [])

> take 5 (repeat 3 ++ repeat 4)
