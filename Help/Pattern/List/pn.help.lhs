concatReplicate :: Int -> [a] -> [a]
pn :: P a -> Int -> P a

Repeats the enclosed pattern a number of times.

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> concatReplicate 4 1
> concatReplicate 4 [1]
> pconcatReplicate 4 1
> pconcatReplicate 4 (fromList [1])

> ln 1 4
> pn 1 4

> ln [1,2,3] 4
> pn (fromList [1,2,3]) 4
