empty :: [a]
pempty :: P a

The empty pattern. (The instance for Monoid mempty.)

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List

> empty :: [()]
> pempty

> [] ++ [1]
> pempty `pappend` return 1
