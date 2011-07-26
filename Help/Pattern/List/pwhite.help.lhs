pwhite :: (Random a) => String -> [a] -> [a] -> [a]

Uniform linear distribution in given range.

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (pwhite 'x' 0.0 1.0)

It is important to note that this structure is not actually
indeterminate, so that the below is zero.

> let p = take 12 (pwhite 'x' 0.0 1.0) in p - p

The below is alternately lower and higher noise.

> let p = pwhite 'x' [0.0,10.0] [1.0,11.0]
> in take 12 p
