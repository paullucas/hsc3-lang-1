module Sound.SC3.Lang.Control.Spec where

import Data.Maybe {- base -}
import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.Lang.Math.Warp as W {- hsc3-lang -}

data Ctl_Spec = Ctl_Spec {cspec_name :: String
                         ,cspec_min :: Double
                         ,cspec_max :: Double
                         ,cspec_warp :: String
                         ,cspec_step :: Double
                         ,cspec_def :: Double
                         ,cspec_unit :: String}
                deriving (Eq,Show)

cspec_warp_f :: Ctl_Spec -> W.Warp Double
cspec_warp_f c =
    let l = cspec_min c
        r = cspec_max c
    in case cspec_warp c of
         "lin" -> W.warpLinear l r
         "exp" -> W.warpExponential l r
         "amp" -> W.warpFader l r
         _ -> error (show ("cspec_warp_f",c))

-- | 'control' 'KR' of 'cspec_name' and 'cspec_def'.
cspec_gen_k :: Ctl_Spec -> UGen
cspec_gen_k c = control KR (cspec_name c) (cspec_def c)

-- | Lookup name and run 'cspec_gen_k'.
cspec_gen_f_maybe :: [Ctl_Spec] -> (String -> Maybe UGen)
cspec_gen_f_maybe c nm =
    let t = zip (map cspec_name c) (control_set (map cspec_gen_k c))
    in lookup nm t

-- | 'error'ing variant.
cspec_gen_f :: [Ctl_Spec] -> (String -> UGen)
cspec_gen_f c nm = fromMaybe (error (show ("cspec_gen_f",nm,c))) (cspec_gen_f_maybe c nm)
