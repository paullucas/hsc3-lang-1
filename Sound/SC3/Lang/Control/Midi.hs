{-# LANGUAGE PackageImports #-}
-- | For a single input controller, key events always arrive in
-- sequence (ie. on->off), ie. for any key on message can allocate an
-- ID and associate it with the key, an off message can retrieve the
-- ID given the key.
module Sound.SC3.Lang.Control.Midi where

import qualified Control.Exception as E
import Control.Monad
import "mtl" Control.Monad.State
import Data.Bits
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.Map as M {- containers -}
import Sound.OpenSoundControl {- hosc -}

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
control_message :: Num a => (a,a,a) -> Control_Message a
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

-- | Join two 7-bit values into a 14-bit value.
--
-- > map (uncurry b_join) [(0,0),(0,64),(127,127)] == [0,8192,16383]
b_join :: Bits a => a -> a -> a
b_join p q = p .|. shiftL q 7

-- | Inverse of 'b_join'.
--
-- > map b_sep [0,8192,16383] == [(0,0),(0,64),(127,127)]
b_sep :: Bits t => t -> (t, t)
b_sep n = (0x7f .&. n,0xff .&. shiftR n 7)

-- | Parse @midi-osc@ @/midi/@ message.
parse_b :: Integral n => Message -> [n]
parse_b m =
    case m of
      Message "/midi" [Int _,Blob b] -> map fromIntegral (B.unpack b)
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

-- | @SC3@ node identifiers are integers.
type Node_Id = Int

-- | Map of allocated 'Node_Id's.
data K a = K (M.Map (a,a) Node_Id) Node_Id

-- | 'StateT' of 'K' specialised to 'Int'.
type KT = StateT (K Int) IO

-- | Initialise 'K' with starting 'Node_Id'.
k_init :: Node_Id -> K a
k_init = K M.empty

-- | 'K' 'Node_Id' allocator.
k_alloc :: (Int,Int) -> KT Node_Id
k_alloc n = do
  (K m i) <- get
  put (K (M.insert n i m) (i + 1))
  return i

-- | 'K' 'Node_Id' retrieval.
k_get :: (Int,Int) -> KT Node_Id
k_get n = do
  (K m _) <- get
  return (m M.! n)

-- | The 'Midi_Receiver' is passed a 'Midi_Message' and a 'Node_Id'.
-- For 'Note_On' and 'Note_Off' messages the 'Node_Id' is positive,
-- for all other message it is @-1@.
type Midi_Receiver m n = Midi_Message n -> Int -> m ()

-- | Parse incoming midi messages, do 'K' allocation, and run
-- 'Midi_Receiver'.
midi_act :: Midi_Receiver IO Int -> Message -> StateT (K Int) IO ()
midi_act f o = do
    let m = parse_m o
    n <- case m of
           Note_Off ch k _ -> k_get (ch,k)
           Note_On ch k _ -> k_alloc (ch,k)
           _ -> return (-1)
    liftIO (f m n)

-- | Run midi system, handles 'E.AsyncException's.
start_midi :: (UDP -> Midi_Receiver IO Int) -> IO ()
start_midi receiver = do
  s_fd <- openUDP "127.0.0.1" 57110 -- midi-osc
  m_fd <- openUDP "127.0.0.1" 57150 -- midi-osc
  sendMessage m_fd (Message "/receive" [Int 0xffff])
  let step = liftIO (recvMessages m_fd) >>=
             midi_act (receiver s_fd) . head
      ex e = print ("start_midi",show (e::E.AsyncException)) >>
             close m_fd >>
             close s_fd
      runs = runStateT (forever step) (k_init 1000) >>
             return ()
  E.catch runs ex
  return ()
