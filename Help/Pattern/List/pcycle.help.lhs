cycle :: [a] -> [a]
pcycle :: P a -> P a

pattern variant of Data.List.cycle

> import Sound.SC3.Lang.Pattern.List

> ptake 5 (pcycle (pseq [1,2,3] 1))

> take 5 (cycle [1,2,3]) == [1,2,3,1,2]

