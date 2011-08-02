psep :: P a -> (P a,P a)
phead :: P a -> P a
ptail :: P a -> P a

Inspect the first element of a pattern.

> import Control.Applicative
> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> fromList [1,2,3]
> fromList [1..]

> psep (fromList [1,2,3])
> psep mempty

The parallel variant.

> phead (fromList [1..] `par` fromList [2..])
> psep (fromList [1,2] `par` fromList [2,3])

> let {p = fromList [1,2,3]
>     ;q = fromList [4,5,6,7,8]
>     ;r = pseq [9,10 `par` 11,12] 1
>     ;s = p `par` q `par` r}
> in plin s

> plin 1
> plin pempty
