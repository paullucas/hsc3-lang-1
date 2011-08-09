take :: Int -> [a] -> [a]
ptake :: Int -> P a -> P a

finval and pfinval are aliases for take and ptake.

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

> take 5 (P.seq [1,2,3] 2)
> take 5 [1,2,3]
> take 5 (P.seq [1,2,3] inf)
> take 5 (white 'a' 0.0 1.0 inf)

> ptake 5 (pseq [1,2,3] 2)
> ptake 5 (fromList [1,2,3])
> ptake 5 (pseq [1,2,3] inf)
> ptake 5 (pwhite 'a' 0.0 1.0 inf)
