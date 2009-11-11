pn :: P a -> P Int -> P a

Repeats the enclosed pattern a number of times.

> import Sound.SC3.Lang.Pattern.List

> pn (pseq [1, 2, 3] 1) 4
