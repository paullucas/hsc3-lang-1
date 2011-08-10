pif :: P Bool -> P a -> P a -> P a

> import Sound.SC3.Lang.Pattern.List as P

> let {a = fmap (< 0.3) (white 'a' 0.0 1.0 inf)
>     ;b = white 'b' 0 9 inf
>     ;c = white 'c' 100 109 inf}
> in take 20 (lif a b c)

> let {a = fmap (< 0.3) (pwhite 'a' 0.0 1.0 inf)
>     ;b = pwhite 'b' 0 9 inf
>     ;c = pwhite 'c' 100 109 inf}
> in ptake 20 (pif a b c)
