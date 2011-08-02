pcycle :: P a -> P a

> ptake 5 (pcycle (pseq [1,2,3] 1))
