pempty :: P a

The empty pattern.

> import Data.Monoid
> import Sound.SC3.Lang.Pattern

> evalP pempty

> evalP (pempty `mappend` return 1)
