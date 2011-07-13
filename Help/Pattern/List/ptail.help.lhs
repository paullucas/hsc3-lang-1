tail' :: [a] -> [a]
ptail :: P a -> P a

Drop first element from pattern.

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List as P

> tail' (P.seq [1,2,3] 1)
> ptail (pseq [1,2,3] 1)

> tail' empty
> ptail pempty

Note that the haskell tail function is partial.

> tail []
