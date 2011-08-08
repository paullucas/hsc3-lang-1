[] :: [a]
pempty :: P a
mempty :: Monoid a => a

The empty pattern. (The instance for Monoid mempty.)

> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List

> [] == mempty
> pempty == mempty

> let e = mempty :: P a
> e `mappend` e
> e `mappend` 1 == 1 `mappend` e
