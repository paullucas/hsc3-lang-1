-- | Midi 'KEY' functions.
module Sound.SC3.Lang.Control.Midi.KY where

import qualified Data.Map as M {- containers -}

-- | @SC3@ node identifiers are integers.
type Node_Id = Int

-- | Map of allocated 'Node_Id's.  For a single input controller, key
-- events always arrive in sequence (ie. on->off), ie. for any key on
-- message we can allocate an ID and associate it with the key, an off
-- message can retrieve the ID given the key.
data KY a = KY (M.Map a [Node_Id]) Node_Id

-- | Initialise 'KY' with starting 'Node_Id'.
ky_init :: Node_Id -> KY a
ky_init = KY M.empty

-- | 'KY' 'Node_Id' allocator.
ky_alloc :: Ord a => KY a -> a -> Int -> (KY a,[Node_Id])
ky_alloc (KY m i) n k = let sq = [i .. i + k - 1] in (KY (M.insert n sq m) (i + k),sq)

-- | Special case to allocate singular ID.
ky_alloc1 :: Ord a => KY a -> a -> (KY a,Node_Id)
ky_alloc1 (KY m i) n = (KY (M.insert n [i] m) (i + 1),i)

-- | 'KY' 'Node_Id' removal.
ky_free :: Ord a => KY a -> a -> (KY a,[Node_Id])
ky_free (KY m i) n =
    let r = m M.! n
    in (KY (M.delete n m) i,r)

-- | Special case to de-allocate singular ID.
ky_free1 :: Ord a => KY a -> a -> (KY a,Node_Id)
ky_free1 ky n = case ky_free ky n of
                  (ky',[x]) -> (ky',x)
                  _ -> error "ky_free1: not singular"

-- | Lookup 'Node_Id'.
ky_get :: Ord a => KY a -> a -> [Node_Id]
ky_get (KY m _) n = m M.! n

-- | All 'Node_Id'.
ky_all :: KY a -> [Node_Id]
ky_all (KY m _) = M.foldl (++) [] m
