cycle :: [a] -> [a]
pcycle :: P a -> P a

pattern variant of Data.List.cycle

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (cycle [1,2,3])
> take 5 (cycle (P.seq [1,2,3] 1))
> ptake 5 (pcycle (pseq [1,2,3] 1))
