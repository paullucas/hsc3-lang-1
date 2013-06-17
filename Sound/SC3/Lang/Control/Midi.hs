-- | Trivial midi functions.
module Sound.SC3.Lang.Control.Midi where

import Control.Exception {- base -}
import Data.Bits {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Sound.OSC.FD {- hosc -}

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

-- * Types

-- | <http://www.midi.org/techspecs/midimessages.php>
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

-- * OSC

-- | Parse @midi-osc@ @/midi/@ message.
parse_b :: Integral n => Message -> [n]
parse_b m =
    case m of
      Message "/midi" [Int32 _,Blob b] -> map fromIntegral (B.unpack b)
      _ -> []

-- | Variant of 'parse_b' that give status byte as low and high.
parse_c :: Integral n => Message -> [n]
parse_c m =
    case parse_b m of
      st:dt -> let (l,h) = st `divMod` 16 in l:h:dt
      _ -> []

-- | Variant of 'parse_c' that constructs a 'Midi_Message'.
parse_m :: (Bits n,Integral n) => Message -> Midi_Message n
parse_m m =
    case parse_c m of
      [0x8,i,j,k] -> Note_Off i j k
      [0x9,i,j,0] -> Note_Off i j 0
      [0x9,i,j,k] -> Note_On i j k
      [0xa,i,j,k] -> Polyphic_Key_Pressure i j k
      [0xb,i,j,k] -> Control_Change i j k
      [0xc,i,j] -> Program_Change i j
      [0xd,i,j] -> Chanel_Aftertouch i j
      [0xe,i,j,k] -> Pitch_Bend i (b_join j k)
      x -> Unknown x

-- * SC3

-- | @SC3@ node identifiers are integers.
type Node_Id = Int

-- | Map of allocated 'Node_Id's.  For a single input controller, key
-- events always arrive in sequence (ie. on->off), ie. for any key on
-- message we can allocate an ID and associate it with the key, an off
-- message can retrieve the ID given the key.
data KY a = KY (M.Map a Node_Id) Node_Id

-- | Initialise 'KY' with starting 'Node_Id'.
ky_init :: Node_Id -> KY a
ky_init = KY M.empty

-- | 'KY' 'Node_Id' allocator.
ky_alloc :: Ord a => KY a -> a -> (KY a,Node_Id)
ky_alloc (KY m i) n = (KY (M.insert n i m) (i + 1),i)

-- | 'KY' 'Node_Id' removal.
ky_free :: Ord a => KY a -> a -> (KY a,Node_Id)
ky_free (KY m i) n =
    let r = m M.! n
    in (KY (M.delete n m) i,r)

-- | Lookup 'Node_Id'.
ky_get :: Ord a => KY a -> a -> Node_Id
ky_get (KY m _) n = m M.! n

-- | All 'Node_Id'.
ky_all :: KY a -> [Node_Id]
ky_all (KY m _) = M.foldl (flip (:)) [] m

-- * IO (midi-osc)

type Midi_Init_f st = (UDP -> IO st)

-- | 'Midi_Recv_f' is passed the @SC3@ connection, the user state, a
-- 'Midi_Message' and, for 'Note_On' and 'Note_Off' messages, a
-- 'Node_Id'.
type Midi_Recv_f st = UDP -> st -> Midi_Message Int -> IO st

-- | Parse incoming midi messages and run 'Midi_Receiver'.
midi_act :: Midi_Recv_f st -> UDP -> st -> Message -> IO st
midi_act recv_f fd st o = do
    let m = parse_m o
    st' <- recv_f fd st m
    return st'

-- | Connect to @midi-osc@ and @sc3@, run initialiser, and then
-- receiver for each incoming message.
run_midi :: Midi_Init_f st -> Midi_Recv_f st -> IO ()
run_midi init_f recv_f = do
  m_fd <- openUDP "127.0.0.1" 57150 -- midi-osc
  s_fd <- openUDP "127.0.0.1" 57110 -- scsynth
  sendMessage m_fd (Message "/receive" [Int32 0xffff])
  init_st <- init_f s_fd
  finally
    (iterateM_ init_st (\st -> recvMessage m_fd >>=
                               midi_act recv_f s_fd st . fromJust))
    (sendMessage m_fd (Message "/receive" [Int32 (-1)]))

-- * Monad

iterateM_ :: (Monad m) => st -> (st -> m st) -> m ()
iterateM_ st f = do
  st' <- f st
  iterateM_ st' f
