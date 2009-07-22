mempty :: P a

The empty pattern.

> import Data.Monoid
> import Sound.SC3.Lang.Pattern

> evalP mempty

> evalP (mempty `mappend` return 1)
