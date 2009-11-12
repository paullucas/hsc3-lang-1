repeat :: a -> [a]
prepeat :: a -> P a

pattern variant of Data.List.repeat

> import Sound.SC3.Lang.Pattern.List

> ptake 5 (prepeat 3)

> take 5 (repeat 3) == [3,3,3,3,3]

