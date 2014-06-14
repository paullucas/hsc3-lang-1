-- | Core (shared) functions.
module Sound.SC3.Lang.Core where

import Data.Maybe {- base -}
import Data.Monoid {- base -}

-- * "Data.Function" variants

-- | 'fmap' '.' 'fmap', ie. @(t -> c) -> (a -> b -> t) -> a -> b -> c@.
(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

-- | 'fmap' '.' '.:', ie. @(t -> d) -> (a -> b -> c -> t) -> a -> b -> c -> d@.
(.::) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(.::) = fmap . (.:)

-- | 'fmap' '.' '.::'.
(.:::) :: (Functor f, Functor g, Functor h,Functor i) => (a -> b) -> f (g (h (i a))) -> f (g (h (i b)))
(.:::) = fmap . (.::)

-- | 'fmap' '.' '.:::'.
(.::::) :: (Functor f, Functor g, Functor h,Functor i,Functor j) => (a -> b) -> f (g (h (i (j a)))) -> f (g (h (i (j b))))
(.::::) = fmap . (.:::)

-- | 'fmap' '.' '.::::'.
(.:::::) :: (Functor f, Functor g, Functor h,Functor i,Functor j,Functor k) => (a -> b) -> f (g (h (i (j (k a))))) -> f (g (h (i (j (k b)))))
(.:::::) = fmap . (.::::)

-- * "Data.List" variants

-- | Variant that either takes precisely /n/ elements or 'Nothing'.
--
-- > map (genericTake 3) (inits "abc") == inits "abc"
-- > Data.Maybe.mapMaybe (genericTakeMaybe 3) (inits "abc") == ["abc"]
genericTakeMaybe :: Integral i => i -> [a] -> Maybe [a]
genericTakeMaybe n l =
    case compare n 0 of
      LT -> Nothing
      EQ -> Just []
      GT -> case l of
              [] -> Nothing
              e : l' -> fmap (e :) (genericTakeMaybe (n - 1) l')

-- | Inverse of 'Data.List.:'.
--
-- > map uncons [[],1:[]] == [(Nothing,[]),(Just 1,[])]
uncons :: [a] -> (Maybe a,[a])
uncons l =
    case l of
      [] -> (Nothing,[])
      x:l' -> (Just x,l')

-- | 'Maybe' variant of '!!'.
--
-- > map (lindex "str") [2,3] == [Just 'r',Nothing]
lindex :: [a] -> Int -> Maybe a
lindex l n =
    if n < 0
    then Nothing
    else case (l,n) of
           ([],_) -> Nothing
           (x:_,0) -> Just x
           (_:l',_) -> lindex l' (n - 1)

-- | If /n/ is 'maxBound' this is 'id', else it is 'take'.
take_inf :: Int -> [a] -> [a]
take_inf n = if n == maxBound then id else take n

-- | Variant of 'transpose' for /fixed width/ interior lists.  Holes
-- are represented by 'Nothing'.
--
-- > transpose_fw undefined [] == []
--
-- > transpose [[1,3],[2,4]] == [[1,2],[3,4]]
-- > transpose_fw 2 [[1,3],[2,4]] == [[Just 1,Just 2],[Just 3,Just 4]]
--
-- > transpose [[1,5],[2],[3,7]] == [[1,2,3],[5,7]]
--
-- > transpose_fw 2 [[1,4],[2],[3,6]] == [[Just 1,Just 2,Just 3]
-- >                                     ,[Just 4,Nothing,Just 6]]
--
-- This function is more productive than 'transpose' for the case of
-- an infinite list of finite lists.
--
-- > map head (transpose_fw 4 (repeat [1..4])) == map Just [1,2,3,4]
-- > map head (transpose (repeat [1..4])) == _|_
transpose_fw :: Int -> [[a]] -> [[Maybe a]]
transpose_fw w l =
    if null l
    then []
    else let f n = map (`lindex` n) l
         in map f [0 .. w - 1]

-- | Variant of 'transpose_fw' with default value for holes.
transpose_fw_def :: a -> Int -> [[a]] -> [[a]]
transpose_fw_def def w l =
    let f n = map (fromMaybe def . (`lindex` n)) l
    in map f [0 .. w - 1]

-- | Variant of 'transpose_fw_def' deriving /width/ from first element.
transpose_fw_def' :: a -> [[a]] -> [[a]]
transpose_fw_def' def l =
    case l of
      [] -> []
      h:_ -> transpose_fw_def def (length h) l

-- | A 'transpose' variant, halting when first hole appears.
--
-- > transpose_st [[1,2,3],[4,5,6],[7,8]] == [[1,4,7],[2,5,8]]
transpose_st :: [[a]] -> [[a]]
transpose_st l =
    let (h,l') = unzip (map uncons l)
    in case all_just h of
         Just h' -> h' : transpose_st l'
         Nothing -> []

-- * Data.Maybe variants

-- | Variant of 'catMaybes' that returns 'Nothing' unless /all/
-- elements are 'Just'.
--
-- > map all_just [[Nothing,Just 1],[Just 0,Just 1]] == [Nothing,Just [0,1]]
all_just :: [Maybe a] -> Maybe [a]
all_just =
    let rec r l =
            case l of
              [] -> Just (reverse r)
              Nothing:_ -> Nothing
              Just e:l' -> rec (e:r) l'
    in rec []

-- * Data.Monoid variants

-- | 'mconcat' of 'repeat', for lists this is 'cycle'.
--
-- > [1,2,3,1,2] `isPrefixOf` take 5 (mcycle [1,2,3])
mcycle :: Monoid a => a -> a
mcycle = mconcat . repeat

