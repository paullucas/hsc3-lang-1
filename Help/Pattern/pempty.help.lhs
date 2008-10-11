pempty :: P a

The empty pattern.

> import Data.Monoid
> import Sound.SC3.Lang.Pattern

> evalP 0 pempty

> evalP 0 (pempty `mappend` return 1)
