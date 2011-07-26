[] :: [a]

The empty pattern. (The instance for Monoid mempty.)

> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List

> []
> [] ++ [1]
> mempty `mappend` [1]
