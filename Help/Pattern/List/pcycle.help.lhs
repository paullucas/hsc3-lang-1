pcycle :: P a -> P a

> import Sound.SC3.Lang.Pattern.List

> take 5 (cycle [1,2,3])
> take 5 (lseq [1,2,3] inf)

> ptake 5 (pcycle (fromList [1,2,3]))
> ptake 5 (pseq [1,2,3] inf)
> ptake 5 (pcycle (pseq [1,2,3] 1))
