module Sound.SC3.Lang.Pattern.Bind where

import Data.List {- base -}
import qualified Data.List.Ordered as O {- data-ordlist -}
import Data.Maybe {- base -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

import qualified Sound.SC3.Lang.Core as L {- hsc3-lang -}

type Param = [(String,[Double])]

sbind_init :: Int -> [Synthdef] -> [Bundle]
sbind_init grp sy =
    let sy_b = bundle 0 (map d_recv sy)
        grp_b = bundle 0 [g_new [(grp,AddToTail,0)]]
    in [sy_b,grp_b]

sbind_tseq :: Int -> [Int] -> (Synthdef,[Time],Maybe [Time],Param) -> [Bundle]
sbind_tseq grp nid (sy,tm,sus,pr) =
    let sy_pr = synthdefParam sy
        has_gate = "gate" `elem` sy_pr
        nd (t,k,ar) = let nm = synthdefName sy
                      in bundle t [s_new nm k AddToTail grp ar]
        pr' = let f (p,l) = zip (repeat p) l
              in L.transpose_st (map f pr)
        gt = if has_gate
             then let sus' = fromMaybe (d_dx' tm) sus
                      -- pr' may be finite, zipped here to halt if required...
                      f (t,g,k,_) = bundle (t + g) [n_set1 k "gate" 0]
                  in map f (zip4 tm sus' nid pr')
             else if isNothing sus
                  then []
                  else error ("sbind_tseq: sus given but no gate parameter")
        pr_unused = (map fst pr \\ sy_pr) \\ ["dur","sustain"]
    in if null pr_unused
       then O.merge (map nd (zip3 tm nid pr')) gt
       else error (show ("sbind_tseq: unused parameters",pr_unused))

sbind_deriv :: Int -> [Int] -> (Synthdef,Param) -> [Bundle]
sbind_deriv grp nid (sy,pr) =
    let dur = fromMaybe (error "sbind_deriv: no dur parameter") (lookup "dur" pr)
        sus = lookup "sustain" pr
        tm = dx_d' dur
    in sbind_tseq grp nid (sy,tm,sus,pr)

sbind :: [(Synthdef,Param)] -> NRT
sbind set =
    let grp = 1
        nid = map (\n -> [n..]) [1000,6000 ..]
    in NRT (sbind_init grp (map fst set) ++ foldl1 O.merge (zipWith (sbind_deriv grp) nid set))

sbind1 :: (Synthdef,Param) -> NRT
sbind1 = sbind . return
