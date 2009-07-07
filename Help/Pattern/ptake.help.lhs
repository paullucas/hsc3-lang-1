ptake :: P Int -> P a -> P a

> import Sound.SC3.Lang.Pattern

> evalP (ptake 5 (pseq [1,2,3] pinf))

> evalP (ptake 5 (pseq [1,2,3] 1))
