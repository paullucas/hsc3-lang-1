module Sound.SC3.Lang.Collection.Collection where

import Prelude
import Data.List

fill :: Int -> (Int -> a) -> [a]
fill n f = map f [0 .. n - 1]

size :: [a] -> Int
size = length

isEmpty :: [a] -> Bool
isEmpty = null

ignoringIndex :: (a -> b) -> a -> Int -> b
ignoringIndex f = \e _ -> f e

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
inject i f l = foldl f i l

any' :: (a -> Int -> Bool) -> [a] -> Bool
any' f l = maybe False (const True) (detect f l)

every :: (a -> Int -> Bool) -> [a] -> Bool
every f l = not (any' g l) where g e i = not (f e i)

count :: (a -> Int -> Bool) -> [a] -> Int
count f l = length (select f l)

occurencesOf :: (Eq a) => a -> [a] -> Int
occurencesOf k l = count (\e _ -> e == k) l

sum' :: (Num a) => (b -> Int -> a) -> [b] -> a
sum' f l = sum (collect f l)

maxItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
maxItem f l = maximum (collect f l)

minItem :: (Ord b) => (a -> Int -> b) -> [a] -> b
minItem f l = minimum (collect f l)
