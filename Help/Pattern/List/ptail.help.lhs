ptail :: [a] -> [a]

Drop first element from pattern.

> import Sound.SC3.Lang.Pattern.List as P

> tail [1,2,3] ==  ptail [1,2,3]

> ptail []

Note that the haskell tail function is partial.

> tail []
