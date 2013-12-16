-- | Core (shared) functions.
module Sound.SC3.Lang.Core where

-- * Function

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

-- * List

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
