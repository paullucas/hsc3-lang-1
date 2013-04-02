-- | An instrument abstraction and a /default/ instrument for patterns.
module Sound.SC3.Lang.Control.Instrument where

import Data.Default {- data-default -}
import Sound.SC3.ID {- hsc3 -}

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

-- | 'Instr' of 'defaultSynthdef', ie. 'def' of 'Synthdef'.
defaultInstr :: Instr
defaultInstr = Instr_Def def True

{-
withSC3 (send (d_recv defaultSynthdef))
-}
