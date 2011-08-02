ptake :: Int -> P a -> P a

> import Sound.SC3.Lang.Pattern.List as P

> ptake 5 (pseq [1,2,3] 2)
> ptake 5 (pseq [1,2,3] 1)
> ptake 5 (pseq [1,2,3] inf)
> ptake 5 (pwhite 'a' 0.0 1.0)
