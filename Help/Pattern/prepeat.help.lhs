prepeat :: a -> P a

pattern variant of Data.List.repeat

> import Sound.SC3.Lang.Pattern

> evalP (ptake 5 (prepeat 3))

[3,3,3,3,3]

