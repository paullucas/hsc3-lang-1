rorate_n :: Num a => [a] -> [a] -> [a]
rorate_l :: Num a => [[a]] -> [a] -> [a]
rorate :: Num a => [Either a [a]] -> [a] -> [a]
prorate :: Num a => P (Either a [a]) -> P a -> P a

> rorate_n [0.35,0.5,0.8] 1
> rorate_n [0.35,0.5,0.8] (rand 'a' [1,20] 3)

> rorate_l (map C.normalizeSum [[1,2],[5,7],[4,8,9]]) 1

> rorate (map Left [0.35,0.5,0.8]) 1
> rorate (map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]]) 1

> prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1
> prorate (fromList (map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]])) 1
