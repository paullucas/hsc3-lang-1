[] :: [a]

The empty pattern. (The instance for Monoid mempty.)

> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List

> let e = mempty::P a
> e `mappend` e
> e `mappend` return 1
