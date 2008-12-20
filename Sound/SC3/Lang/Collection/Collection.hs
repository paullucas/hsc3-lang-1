module Sound.SC3.Lang.Collection.Collection where

import Data.List
import Data.Maybe

fill :: Int -> (Int -> a) -> [a]
fill n f = map f [0 .. n - 1]

size :: [a] -> Int
size = length

isEmpty :: [a] -> Bool
isEmpty = null

ignoringIndex :: (a -> b) -> a -> Int -> b
ignoringIndex f e _ = f e

collect :: (a -> Int -> b) -> [a] -> [b]
collect f l = zipWith f l [0..]

select :: (a -> Int -> Bool) -> [a] -> [a]
select f l = map fst (filter (uncurry f) (zip l [0..]))

reject :: (a -> Int -> Bool) -> [a] -> [a]
reject f l = map fst (filter (not . uncurry f) (zip l [0..]))

detect :: (a -> Int -> Bool) -> [a] -> Maybe a
detect f l = maybe Nothing (Just . fst) (find (uncurry f) (zip l [0..]))

detectIndex :: (a -> Int -> Bool) -> [a] -> Maybe Int
detectIndex f l = maybe Nothing (Just . snd) (find (uncurry f) (zip l [0..]))

inject :: a -> (a -> b -> a) -> [b] -> a
inject i f = foldl f i

any' :: (a -> Int -> Bool) -> [a] -> Bool
any' f = isJust . detect f

every :: (a -> Int -> Bool) -> [a] -> Bool
every f = let g e = not . f e
          in not . any' g

count :: (a -> Int -> Bool) -> [a] -> Int
count f = length . select f

occurencesOf :: (Eq a) => a -> [a] -> Int
occurencesOf k = count (\e _ -> e == k)

sum' :: (Num a) => (b -> Int -> a) -> [b] -> a
sum' f = sum . collect f

maxItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
maxItem f = maximum . collect f

minItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
minItem f = minimum . collect f
