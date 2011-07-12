countpre :: [Bool] -> [Int]
countpost :: [Bool] -> [Int]
pcountpre :: P Bool -> P Int
pcountpost :: P Bool -> P Int

> import Sound.SC3.Lang.Pattern.List

> countpre (bool [0,0,1,0,0,0,1,1])
> pcountpre (pbool (pseq [0,0,1,0,0,0,1,1] 1))

> countpost (bool [1,0,1,0,0,0,1,1])
> pcountpost (pbool (pseq [1,0,1,0,0,0,1,1] 1))
