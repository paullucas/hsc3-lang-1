module Sound.SC3.Lang.Pattern.List where

import Data.List
import Sound.SC3.Lang.Pattern.Pattern
import Sound.SC3.Lang.Pattern.Control

pseq_ :: [P a] -> Int -> P a
pseq_ l n = plist (concat (replicate n l))

pseq :: [P a] -> P Int -> P a
pseq l n = n >>= (\x -> plist (concat (replicate x l)))

-- | 'n' values from the infinite cycle of the streams at l.
pser_ :: [P a] -> Int -> P a
pser_ l n = prestrict_ n (plist l)

pser :: [P a] -> P Int -> P a
pser l n = prestrict n (plist l)

pswitch :: [P a] -> P Int -> P a
pswitch l i = i >>= (l !!)

pswitch1 :: [P a] -> P Int -> P a
pswitch1 = undefined

ppatlace :: [P a] -> P Int -> P a
ppatlace = undefined
