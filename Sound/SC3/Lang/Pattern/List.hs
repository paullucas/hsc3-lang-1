module Sound.SC3.Lang.Pattern.List where

import qualified Data.IntMap as M
import Data.List
import Data.Monoid
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

pswitch1m :: M.IntMap (P a) -> P Int -> P a
pswitch1m m is = let f i js = let h = phead (m M.! i)
                                  t = ptail (m M.! i)
                              in h `mappend` pswitch1m (M.insert i t m) js
                 in pcontinue is f

pswitch1 :: [P a] -> P Int -> P a
pswitch1 = pswitch1m . M.fromList . zip [0..]

ppatlace :: [P a] -> P Int -> P a
ppatlace ps n = let is = pseq (map return [0 .. length ps - 1]) pinf
                in ptake n (pswitch1 ps is)

{-

Neither the definition above or the variant below are correct.
Both deadlock once all patterns are empty.  pswitch1 has the 
same problem.  

ppatlacea :: P (P a) -> P a
ppatlacea ps = 
    let f p qs = let h = phead p
                     t = ptail p
                     rs = qs `mappend` return t
                 in h `mappend` (ppatlacea rs)
    in pcontinue ps f
-}
