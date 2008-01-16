pwhite :: (Random a) => P a -> P a -> P Int -> P a

Uniform linear distribution in given range.

> let p = pwhite 0.0 1.0 12
> in evalP 0 p

> let { l = pseq [0.0, 10.0] 1
>     ; r = pseq [1.0, 11.0] 1
>     ; p = pwhite l r 12 }
> in evalP 0 p
