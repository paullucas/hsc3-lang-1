-- | @sclang@ value pattern functions.
-- See <http://rd.slavepianos.org/?t=hsc3-texts> for tutorial.
--
-- Haskell patterns: `pappend`, `pbool`, `pconcat`, `pcons`,
-- `pcountpost`, `pcountpre`, `pcycle`, `pempty`,`pfilter`, `phold`,
-- `pinterleave`,`pjoin`, `prepeat`, `preplicate`, `prsd`, `pscanl`,
-- `psplitPlaces`, `psplitPlaces'`, `ptail`, `ptake`, `ptrigger`,
-- `pzip`, `pzipWith`
module Sound.SC3.Lang.Pattern.P.Base where

import Control.Applicative {- base -}
import Control.Monad {- base -}
import qualified Data.Foldable as F {- base -}
import qualified Data.List as L {- base -}
import qualified Data.List.Split as S {- split -}
import Data.Monoid {- base -}
import qualified Data.Traversable as T {- base -}

import Sound.SC3.Lang.Pattern.P.Core

import Sound.SC3.Lang.Core
import qualified Sound.SC3.Lang.Pattern.List as P
import qualified Sound.SC3.Lang.Pattern.Stream as I

-- * Math

-- | Type specialised 'maxBound', a pseudo-/infinite/ value for use at
-- pattern repeat counts.
--
-- > inf == maxBound
inf :: Int
inf = maxBound

{-| Constant /NaN/ (not a number) value.

> isNaN nan == True

A frequency value of NaN indicates a rest. This constant value can be
used as a rest indicator at a frequency model input (not at a @rest@
key).

> audition (pbind [(K_dur,pseq [0.1,0.7] inf)
>                 ,(K_legato,0.2)
>                 ,(K_degree,pseq [0,2,return nan] inf)])

-}
nan :: Floating a => a
nan = sqrt (-1)

-- * Data.List Patterns

-- | Pattern variant of 'Data.List.:'.
--
-- > pcons 'α' (pn (return 'β') 2) == toP "αββ"
pcons :: a -> P a -> P a
pcons = mappend . return

-- | Pattern variant of 'Data.List.null'.
--
-- > pnull mempty == True
-- > pnull (undecided 'a') == False
-- > pnull (pure 'a') == False
-- > pnull (return 'a') == False
pnull :: P a -> Bool
pnull = null . unP

-- | Alias for 'pure', pattern variant of 'Data.List.repeat'.
--
-- > ptake 5 (prepeat 3) == toP [3,3,3,3,3]
-- > ptake 5 (pure 3) == toP [3,3,3,3,3]
-- > take 5 (pure 3) == [3]
prepeat :: a -> P a
prepeat = pure

-- | Pattern variant of 'splitAt'.
psplitAt :: Int -> P a -> (P a,P a)
psplitAt n p =
    let (i,j) = splitAt n (unP p)
    in (toP i,toP j)

-- * Data.List.Split

-- | Pattern variant of 'Data.List.Split.splitPlaces'.
--
-- > psplitPlaces' (toP [1,2,3]) (pseries 1 1 6) == toP [[1],[2,3],[4,5,6]]
-- > psplitPlaces' (toP [1,2,3]) (toP ['a'..]) == toP ["a","bc","def"]
psplitPlaces' :: P Int -> P a -> P [a]
psplitPlaces' = liftP2 S.splitPlaces

-- | 'fmap' 'toP' of 'psplitPlaces''.
--
-- > psplitPlaces (toP [1,2,3]) (toP ['a'..]) == toP (map toP ["a","bc","def"])
psplitPlaces :: P Int -> P a -> P (P a)
psplitPlaces = fmap toP .: psplitPlaces'

-- | Pattern variant of 'take_inf', see also 'pfinval'.
--
-- > ptake 5 (pseq [1,2,3] 2) == toP [1,2,3,1,2]
-- > ptake 5 (toP [1,2,3]) == toP [1,2,3]
-- > ptake 5 (pseq [1,2,3] inf) == toP [1,2,3,1,2]
-- > ptake 5 (pwhite 'α' 0 5 inf) == toP [5,2,1,2,0]
--
-- Note that `ptake` does not extend the input pattern, unlike `pser`.
--
-- > ptake 5 (toP [1,2,3]) == toP [1,2,3]
-- > pser [1,2,3] 5 == toP [1,2,3,1,2]
ptake :: Int -> P a -> P a
ptake n = liftP (take_inf n)

-- | Type specialised 'mcycle'.
--
-- > ptake 5 (pcycle 1) == preplicate 5 1
-- > ptake 5 (pcycle (pure 1)) == preplicate 5 1
-- > ptake 5 (pcycle (return 1)) == preplicate 5 1
pcycle :: P a -> P a
pcycle = mcycle

-- | Type specialised 'mfilter'.  Aliased to `pselect`.  See also
-- `preject`.
--
-- > mfilter even (pseq [1,2,3] 2) == toP [2,2]
-- > mfilter (< 3) (pseq [1,2,3] 2) == toP [1,2,1,2]
pfilter :: (a -> Bool) -> P a -> P a
pfilter = mfilter

-- | Pattern variant of `replicate`.
--
-- > preplicate 4 1 == toP [1,1,1,1]
--
-- Compare to `pn`:
--
-- > pn 1 4 == toP [1,1,1,1]
-- > pn (toP [1,2]) 3 == toP [1,2,1,2,1,2]
-- > preplicate 4 (toP [1,2]) :: P (P Int)
preplicate :: Int -> a -> P a
preplicate n = toP . (if n == inf then repeat else replicate n)

-- | Pattern variant of `scanl`.  `scanl` is similar to `foldl`, but
-- returns a list of successive reduced values from the left.  pscanl
-- is an accumulator, it provides a mechanism for state to be threaded
-- through a pattern. It can be used to write a function to remove
-- succesive duplicates from a pattern, to count the distance between
-- occurences of an element in a pattern and so on.
--
-- > F.foldl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == 43
-- > pscanl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1) == toP [4,9,20,43]
--
-- > F.foldl (flip (:)) [] (toP [1..3]) == [3,2,1]
-- > pscanl (flip (:)) [] (toP [1..3]) == toP [[],[1],[2,1],[3,2,1]]
--
-- > F.foldl (+) 0 (toP [1..5]) == 15
-- > pscanl (+) 0 (toP [1..5]) == toP [0,1,3,6,10,15]
pscanl :: (a -> b -> a) -> a -> P b -> P a
pscanl f i = liftP (L.scanl f i)

-- | 'pdrop' @1@.  Pattern variant of `Data.List.tail`.  Drops first
-- element from pattern.  Note that the haskell `tail` function is
-- partial, although `drop` is not.  `ptake` is equal to `pdrop 1`.
--
-- > tail [] == _|_
-- > drop 1 [] == []
--
-- > ptail (toP [1,2]) == toP [2]
-- > ptail mempty == mempty
ptail :: P a -> P a
ptail = liftP (drop 1)

-- | Variant of 'L.transpose'.
--
-- > L.transpose [[1,2],[3,4,5]] == [[1,3],[2,4],[5]]
-- > ptranspose [toP [1,2],toP [3,4,5]] == toP [[1,3],[2,4],[5]]
--
-- > let p = ptranspose [pseq [1,2] inf,pseq [4,5] inf]
-- > in ptake 2 (pdrop (2^16) p) == toP [[1,4],[2,5]]
ptranspose :: [P a] -> P [a]
ptranspose l = toP (L.transpose (map unP l))

-- | An /implicitly repeating/ pattern variant of 'transpose_st'.
ptranspose_st_repeat :: [P a] -> P [a]
ptranspose_st_repeat l = toP (transpose_st (map unP_repeat l))

-- * Non-SC3 Patterns

-- | Type specialised 'P.fbool'.
pbool :: (Ord a,Num a) => P a -> P Bool
pbool = P.fbool

-- | 'mconcat' of 'replicate'.
pconcatReplicate :: Int -> P a -> P a
pconcatReplicate = mconcat .: replicate

-- | Lifted 'P.countpost'.
pcountpost :: P Bool -> P Int
pcountpost = liftP P.countpost

-- | Lifted 'P.countpre'.
pcountpre :: P Bool -> P Int
pcountpre = liftP P.countpre

-- | Lifted 'P.hold'.
phold :: P a -> P a
phold = liftP P.hold

-- | Lifted 'P.interleave2'.
--
-- > let p = pinterleave2 (pwhite 'α' 1 9 inf) (pseries 10 1 5)
-- > in [3,10,9,11,2,12,9,13,4,14] `L.isPrefixOf` unP p
pinterleave2 :: P a -> P a -> P a
pinterleave2 = liftP2 P.interleave2

-- | Lifted 'P.interleave'.
--
-- > pinterleave [pwhitei 'α' 0 4 3,pwhitei 'β' 5 9 3] == toP [2,7,0,5,3,6]
pinterleave :: [P a] -> P a
pinterleave = toP . P.interleave . map unP

-- | Lifted 'L.isPrefixOf'.
pisPrefixOf :: Eq a => P a -> P a -> Bool
pisPrefixOf p q = L.isPrefixOf (unP p) (unP q)

-- | Lifted 'I.rsd'.
--
-- > prsd (pstutter 2 (toP [1,2,3])) == toP [1,2,3]
-- > prsd (pseq [1,2,3] 2) == toP [1,2,3,1,2,3]
prsd :: (Eq a) => P a -> P a
prsd = liftP I.rsd

-- | Lifted 'P.trigger'.
--
-- > let {tr = pbool (toP [0,1,0,0,1,1])
-- >     ;p = ptrigger tr (toP [1,2,3])
-- >     ;r = [Nothing,Just 1,Nothing,Nothing,Just 2,Just 3]}
-- > in p == toP r
ptrigger :: P Bool -> P a -> P (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i x = preplicate i Nothing <> return (Just x)
    in join (pzipWith f r q)

-- * Aliases

-- | Type specialised 'mappend', sequences two patterns,
-- ie. 'Data.List.++'.
--
-- > 1 <> mempty <> 2 == toP [1,2]
--
-- > let {p = prand 'α' [0,1] 3
-- >     ;q = prand 'β' [5,7] 3}
-- > in audition (pbind [(K_degree,pappend p q),(K_dur,0.15)])
pappend :: P a -> P a -> P a
pappend = mappend

-- | Type specialised 'mconcat' (or equivalently 'msum' or
-- 'Data.List.concat').
--
-- > mconcat [pseq [1,2] 1,pseq [3,4] 2] == toP [1,2,3,4,3,4]
-- > msum [pseq [1,2] 1,pseq [3,4] 2] == toP [1,2,3,4,3,4]
pconcat :: [P a] -> P a
pconcat = mconcat

-- | Type specialised `mempty`, ie. 'Data.List.[]'.
pempty :: P a
pempty = mempty

-- | Type specialised 'F.foldr'.
--
-- > > (Pser([1,2,3],5) + Pseq([0,10],3)).asStream.all == [1,12,3,11,2]
--
-- > let p = pser [1,2,3] 5 + pseq [0,10] 3
-- > in F.foldr (:) [] p == [1,12,3,11,2]
--
-- Indefinte patterns may be folded.
--
-- > take 3 (F.foldr (:) [] (prepeat 1)) == [1,1,1]
--
-- The `Foldable` module includes functions for 'F.product', 'F.sum',
-- 'F.any', 'F.elem' etc.
--
-- > F.product (toP [1,3,5]) == 15
-- > floor (F.sum (ptake 100 (pwhite 'α' 0.25 0.75 inf))) == 51
-- > F.any even (toP [1,3,5]) == False
-- > F.elem 5 (toP [1,3,5]) == True
pfoldr :: (a -> b -> b) -> b -> P a -> b
pfoldr = F.foldr

-- | Type specialised 'join'.
--
-- > join (replicate 2 [1,2]) == [1,2,1,2]
-- > join (preplicate 2 (toP [1,2])) == toP [1,2,1,2]
pjoin :: P (P a) -> P a
pjoin = join

-- | 'join' that pushes an outer 'undecided' inward.
--
-- > join (undecided (undecided 1)) == undecided 1
-- > join (undecided (return 1)) == return 1
-- > pjoin_repeat (undecided (return 1)) == pure 1 == _|_
pjoin_repeat :: P (P a) -> P a
pjoin_repeat p =
    case p of
      P (Left (P (Right l))) -> toP (cycle l)
      _ -> join p

-- | Type specialised 'fmap', ie. 'Data.List.map'.
pmap :: (a -> b) -> P a -> P b
pmap = fmap

-- | Type specialised '>>='.
--
-- > (return 1 >>= return . id) == [1]
-- > (undecided 1 >>= undecided . id) == undecided 1
--
-- > (pseq [1,2] 1 >>= \x ->
-- >   pseq [3,4,5] 1 >>= \y ->
-- >    return (x,y)) == toP [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
pmbind :: P a -> (a -> P b) -> P b
pmbind = (>>=)

-- | Type specialised 'pure'.
ppure :: a -> P a
ppure = pure

-- | Type specialised 'return'.
preturn :: a -> P a
preturn = return

-- | Type specialised 'T.traverse'.
--
-- > let {f i e = (i + e,e * 2)
-- >     ;(r,p) = T.mapAccumL f 0 (toP [1,3,5])}
-- > in (r,p) == (9,toP [2,6,10])
ptraverse :: Applicative f => (a -> f b) -> P a -> f (P b)
ptraverse = T.traverse
