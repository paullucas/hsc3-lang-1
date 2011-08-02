pn :: P a -> Int -> P a

Repeats the enclosed pattern a number of times.

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> pn (fromList [1,2,3]) 4
> pn 1 4
