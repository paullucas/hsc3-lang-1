pempty :: P a

The empty pattern. (The instance for Monoid mempty.)

> import Sound.SC3.Lang.Pattern.List

> pempty

> pempty `pappend` return 1
