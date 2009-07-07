pcountpre :: P Bool -> P Int
pcountpost :: P Bool -> P Int

> import Sound.SC3.Lang.Pattern

> let p = pbool (pseq [0, 0, 1, 0, 0, 0, 1, 1] 1)
> in evalP (pcountpre p)

> let p = pbool (pseq [1, 0, 1, 0, 0, 0, 1, 1] 1)
> in evalP (pcountpost p)
