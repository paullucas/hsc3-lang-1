-- | Trivial midi functions.
module Sound.SC3.Lang.Control.Midi where

import Data.Bits {- base -}

-- * Bits

-- | Join two 7-bit values into a 14-bit value.
--
-- > map (uncurry b_join) [(0,0),(0,64),(127,127)] == [0,8192,16383]
b_join :: Bits a => a -> a -> a
b_join p q = p .|. shiftL q 7

-- | Inverse of 'b_join'.
--
-- > map b_sep [0,8192,16383] == [(0,0),(0,64),(127,127)]
b_sep :: (Num t,Bits t) => t -> (t, t)
b_sep n = (0x7f .&. n,0xff .&. shiftR n 7)

-- > status_sep [0x90,60,64] == [9,0,60,64]
status_sep :: Integral t => [t] -> [t]
status_sep m =
    case m of
      [] -> []
      st:dt -> let (l,h) = st `divMod` 16 in l:h:dt

status_join :: Integral t => [t] -> [t]
status_join m =
    case m of
      (st:ch:dt) -> st * 0x10 + ch : dt
      _ -> error "status_join"

-- * Types

-- | <http://www.midi.org/techspecs/midimessages.php>
--
-- Channel messages have the channel number in the first position.
data Midi_Message a = Chanel_Aftertouch a a
                    | Control_Change a a a
                    | Note_On a a a
                    | Note_Off a a a
                    | Polyphic_Key_Pressure a a a
                    | Program_Change a a
                    | Pitch_Bend a a
                    | Unknown [a]
                      deriving (Eq,Show)

-- | 'Control_Change' midi messages have, in some cases, commonly
-- defined meanings.
data Control_Message a = All_Notes_Off a
                       | All_Sound_Off a
                       | Balance a a
                       | Bank_Select a a
                       | Breath_Controller a a
                       | Expression_Controller a a
                       | Foot_Controller a a
                       | Local_Control a a
                       | Modulation_Wheel a a
                       | Mono_Mode_On a a
                       | Omni_Mode_Off a
                       | Omni_Mode_On a
                       | Pan a a
                       | Poly_Mode_On a
                       | Portamento_On_Off a a
                       | Portamento_Time a a
                       | Reset_All_Controllers a a
                       | Soft_Pedal_On_Off a a
                       | Sostenuto_On_Off a a
                       | Sustain_On_Off a a
                       | Undefined
                       | Volume a a
                         deriving (Eq,Show)

-- | 'Control_Change' midi messages may, in some cases, have commonly
-- defined meanings.
--
-- > control_message (0,123,0) == All_Notes_Off 0
control_message :: (Eq a,Num a) => (a,a,a) -> Control_Message a
control_message (i,j,k) =
    case j of
      0 -> Bank_Select i k
      1 -> Modulation_Wheel i k
      2 -> Breath_Controller i k
      4 -> Foot_Controller i k
      5 -> Portamento_Time i k
      7 -> Volume i k
      8 -> Balance i k
      10 -> Pan i k
      11 -> Expression_Controller i k
      64 -> Sustain_On_Off i k
      65 -> Portamento_On_Off i k
      66 -> Sostenuto_On_Off i k
      67 -> Soft_Pedal_On_Off i k
      120 -> All_Sound_Off i
      121 -> Reset_All_Controllers i k
      122 -> Local_Control i j
      123 -> All_Notes_Off i
      124 -> Omni_Mode_Off i
      125 -> Omni_Mode_On i
      126 -> Mono_Mode_On i j
      127 -> Poly_Mode_On i
      _ -> Undefined

-- * Encoding & Decoding

-- | Byte sequence encoding for 'Midi_Message'.
--
-- > m_encode (Control_Change 0 16 127) == [176,16,127]
m_encode :: (Bits t, Integral t) => Midi_Message t -> [t]
m_encode m =
    let r = case m of
              Chanel_Aftertouch i j -> [0xd,i,j]
              Control_Change i j k -> [0xb,i,j,k]
              Note_On i j k -> [0x9,i,j,k]
              Note_Off i j k -> [0x8,i,j,k]
              Polyphic_Key_Pressure i j k -> [0xa,i,j,k]
              Program_Change i j -> [0xc,i,j]
              Pitch_Bend i j -> let (p,q) = b_sep j in [0xe,i,p,q]
              Unknown x -> x
    in status_join r

m_decode :: (Integral t,Bits t) => [t] -> Midi_Message t
m_decode m =
    case m of
      [0x8,i,j,k] -> Note_Off i j k
      [0x9,i,j,0] -> Note_Off i j 0
      [0x9,i,j,k] -> Note_On i j k
      [0xa,i,j,k] -> Polyphic_Key_Pressure i j k
      [0xb,i,j,k] -> Control_Change i j k
      [0xc,i,j] -> Program_Change i j
      [0xd,i,j] -> Chanel_Aftertouch i j
      [0xe,i,j,k] -> Pitch_Bend i (b_join j k)
      x -> Unknown x

