take :: Int -> [a] -> [a]
ptake :: P Int -> P a -> P a

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (P.seq [1,2,3] 5)
> ptake 5 (pseq [1,2,3] 5)

> take 5 (P.seq [1,2,3] 1)
> ptake 5 (pseq [1,2,3] 1)

