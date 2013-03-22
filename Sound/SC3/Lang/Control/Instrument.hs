-- | An instrument abstraction and a /default/ instrument for patterns.
module Sound.SC3.Lang.Control.Instrument where

import Sound.SC3.ID

-- | An 'Instr' is either a 'Synthdef' or the 'String' naming a
-- 'Synthdef'.
data Instr = Instr_Def {i_def :: Synthdef,i_send_release :: Bool}
           | Instr_Ref {i_ref :: String,i_send_release :: Bool}
             deriving (Eq,Show)

-- | All 'Instr' have a name.
i_name :: Instr -> String
i_name i =
    case i of
      Instr_Def s _ -> synthdefName s
      Instr_Ref nm _ -> nm

-- | All 'Instr' may have a 'Synthdef'.
i_synthdef :: Instr -> Maybe Synthdef
i_synthdef i =
    case i of
      Instr_Def s _ -> Just s
      Instr_Ref _ _ -> Nothing

-- | If 'I_Def' subsequent are 'I_Ref', else all 'I_Ref'.
i_repeat :: Instr -> [Instr]
i_repeat i =
    case i of
      Instr_Def d sr -> i : repeat (Instr_Ref (synthdefName d) sr)
      Instr_Ref _ _ -> repeat i

-- | The SC3 /default/ instrument 'Synthdef'.
defaultSynthdef :: Synthdef
defaultSynthdef =
    let f = control KR "freq" 440
        a = control KR "amp" 0.1
        p = control KR "pan" 0
        g = control KR "gate" 1
        e = linen g 0.01 0.7 0.3 RemoveSynth
        f3 = mce [f,f + rand 'a' (-0.4) 0,f + rand 'b' 0 0.4]
        l = xLine KR (rand 'c' 4000 5000) (rand 'd' 2500 3200) 1 DoNothing
        z = lpf (mix (varSaw AR f3 0 0.3 * 0.3)) l * e
    in synthdef "default" (out 0 (pan2 z p a))

-- | 'Instr' of 'defaultSynthdef'.
defaultInstr :: Instr
defaultInstr = Instr_Def defaultSynthdef True

{-
withSC3 (send (d_recv defaultSynthdef))
-}
