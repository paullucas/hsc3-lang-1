-- | Infinte list @SC3@ pattern functions.
module Sound.SC3.Lang.Pattern.Stream where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.Random {- random -}

import qualified Sound.SC3 as S {- hsc3 -}

import Sound.SC3.Lang.Core {- hsc3-lang -}
import qualified Sound.SC3.Lang.Math as M {- hsc3-lang -}
import qualified Sound.SC3.Lang.Random.Gen as R

-- | Remove successive duplicates.
--
-- > rsd [1,1,2,2,3,3] == [1,2,3]
-- > rsd [1,2,3,1,2,3] == [1,2,3,1,2,3]
rsd :: Eq a => [a] -> [a]
rsd =
    let f (p,_) i = (Just i,if Just i == p then Nothing else Just i)
    in mapMaybe snd . scanl f (Nothing,Nothing)

-- | True if /a/ is initially equal to /b/.
iEq :: Eq a => [a] -> [a] -> Bool
iEq = flip isPrefixOf

-- | Take elements from /l/ until all elements in /s/ have been seen.
-- If /s/ contains duplicate elements these must be seen multiple
-- times.
--
-- > take_until_forms_set "abc" "a random sentence beginning" == "a random sentence b"
take_until_forms_set :: Eq a => [a] -> [a] -> [a]
take_until_forms_set s l =
    if null s
    then []
    else case l of
           [] -> []
           e:l' -> e : take_until_forms_set (delete e s) l'

-- | Underlying 'brown'.
brown_ :: (RandomGen g,Random n,Num n,Ord n) => (n,n,n) -> (n,g) -> (n,g)
brown_ (l,r,s) (n,g) =
    let (i,g') = randomR (-s,s) g
    in (S.foldToRange l r (n + i),g')

brown' :: (RandomGen g,Num t,Ord t,Random t) => (t,g) -> [t] -> [t] -> [t] -> [t]
brown' i l_ r_ s_ =
    let rec (n,g) z =
            case z of
              [] -> []
              (l,r,s):z' -> let (n',g') = brown_ (l,r,s) (n,g)
                            in n' : rec (n',g') z'
    in rec i (zip3 l_ r_ s_)

-- | Brown noise with list inputs and random intial value.
--
-- > let l = brown 'α' (repeat 1) (repeat 700) (cycle [1,20])
-- > in l `iEq` [415,419,420,428]
brown :: (Enum e,Random n,Num n,Ord n) => e -> [n] -> [n] -> [n] -> [n]
brown e l_ r_ = brown' (randomR (head l_,head r_) (mkStdGen (fromEnum e))) l_ r_

-- | 'M.exprange' of 'white'
exprand :: (Enum e,Random a,Floating a) => e -> a -> a -> [a]
exprand e l r = fmap (M.exprange l r) (white e 0 1)

-- | Geometric series.
--
-- > geom 3 6 `iEq` [3,18,108,648,3888,23328,139968]
geom :: Num a => a -> a -> [a]
geom i s = iterate (* s) i

-- > lace [[0],[1,2],[3,4,5]] `iEq` [0,1,3,0,2,4,0,1,5]
-- > lace [[1],[2,5],[3,6]] `iEq` [1,2,3,1,5,6]
-- > lace [[1],[2,5],[3,6..]] `iEq` [1,2,3,1,5,6,1,2,9,1,5,12]
lace :: [[a]] -> [a]
lace = concat . transpose . map cycle

-- | Random elements from list.
--
-- > take_until_forms_set "string" (rand 'α' "string") == "grtrsiirn"
rand :: Enum e => e -> [a] -> [a]
rand e a =
    let k = length a - 1
    in map (a !!) (white e 0 k)

-- | List section with /wrapped/ indices.
--
-- > segment [0..4] 5 (3,5) == [3,4,0]
segment :: [a] -> Int -> (Int,Int) -> [a]
segment a k (l,r) =
    let i = map (S.generic_wrap 0 (k - 1)) [l .. r]
    in map (a !!) i

-- > slide [1,2,3,4] 4 1 0 True `iEq` [[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]
-- > slide [1,2,3,4,5] 3 (-1) 0 True `iEq` [[1,2,3],[5,1,2],[4,5,1],[3,4,5],[2,3,4]]
slide :: [a] -> Int -> Int -> Int -> Bool -> [[a]]
slide a j s i wr =
    let k = length a
        l = enumFromThen i (i + s)
        r = map (+ (j - 1)) l
    in if wr
       then map (segment a k) (zip l r)
       else error "slide: non-wrap variant not implemented"

-- | 'concat' of 'slide'.
slidec :: [a] -> Int -> Int -> Int -> Bool -> [a]
slidec = concat .:::: slide

-- | White noise.
--
-- > take_until_forms_set [1..5] (white 'α' 1 5) == [4,1,2,2,2,1,2,1,2,5,1,4,3]
white :: (Random n,Enum e) => e -> n -> n -> [n]
white e l r = randomRs (l,r) (mkStdGen (fromEnum e))

-- | Weighted selection of elements from a list.
wrand_generic :: (Enum e,Fractional n,Ord n,Random n) => e -> [a] -> [n] -> [a]
wrand_generic e a w =
    let f g = let (r,g') = R.wchoose a w g
              in r : f g'
    in if length a /= length w
       then error "wrand_generic: a/w must be of equal length"
       else f (mkStdGen (fromEnum e))

-- | Type restricted variant.
--
-- > import qualified Sound.SC3.Lang.Collection as C
--
-- > let {w = C.normalizeSum [1..5]
-- >     ;r = wrand 'ζ' "wrand" w}
-- > in take_until_forms_set "wrand" r == "dnanrdnaddrnrrndrrdw"
wrand :: Enum e => e -> [a] -> [Double] -> [a]
wrand = wrand_generic

-- | Select elements from /l/ in random sequence, but do not immediately repeat an element.
--
-- > take_until_forms_set "string" (xrand 'α' "string") == "grtrsirn"
xrand :: Enum e => e -> [a] -> [a]
xrand e a =
    let g = mkStdGen (fromEnum e)
        k = length a - 1
        r = rsd (randomRs (0,k) g)
    in map (a !!) r
