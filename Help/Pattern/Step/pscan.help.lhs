pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P y -> P a

Basic state threading function.  x is the state, an optional
final state to value function can be given if required.

> import Sound.SC3.Lang.Pattern.Step

> let { p = pzip (pbool (pseq [1,0,0,1,0] 1)) (pseq [1,2,3,4,5] 1)
>     ; f ys (b, x) = let r = if b then [x] else (x:ys) in (r, r) }
> in evalP (pscan f Nothing [] p)

[[1],[2,1],[3,2,1],[4],[5,4]]

> let { b = pbool (pseq [1,0,0,1,0,1] 1)
>     ; p = pseq [1,2,3] 1
>     ; q = pseq [11,12,13] 1
>     ; f (x, y) True = ((ptail x, y), phead x)
>     ; f (x, y) False = ((x, ptail y), phead y) }
> in evalP (psequence (pscan f Nothing (p,q) b))

[1,11,12,2,13,3]

> let { p = pbool (pseq [1,0,0,1,0,1] 1)
>     ; q = pseq [1,2,3] 2
>     ; r = pseq [11,12,13] 2
>     ; s = pzip3 p q r
>     ; f ([],ys) (True, x, y) = (([], ys ++ [y]), x)
>     ; f ((x':xs),ys) (True, x, y) = ((xs ++ [x], ys ++ [y]), x')
>     ; f (xs,[]) (False, x, y) = ((xs ++ [x], []), y)
>     ; f (xs,y':ys) (False, x, y) = ((xs++[x], ys ++ [y]), y') }
> in evalP (pscan f Nothing ([],[]) s)

[1,11,12,2,13,3]
