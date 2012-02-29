{-# LANGUAGE PackageImports #-}
-- | For a single input controller, key events always arrive in
-- sequence (ie. on->off), ie. for any key an on message can allocate
-- an ID and associate it with the key, an off message can retrieve
-- the ID given the key.
module Sound.SC3.Lang.Control.Midi where

import qualified Control.Exception as E
import "mtl" Control.Monad.State
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.Map as M {- containers -}
import Sound.OpenSoundControl {- hosc -}

-- | <http://www.midi.org/techspecs/midimessages.php>
data Midi_Message a = All_Notes_Off a
                    | All_Sound_Off a
                    | Chanel_Aftertouch a a
                    | Control_Change a a a
                    | Local_Control a a
                    | Mono_Mode_On a a
                    | Note_On a a a
                    | Note_Off a a a
                    | Omni_Mode_Off a
                    | Omni_Mode_On a
                    | Poly_Mode_On a
                    | Polyphic_Key_Pressure a a a
                    | Program_Change a a
                    | Pitch_Bend a a
                    | Reset_All_Controllers a a
                    | Unknown [a]
                      deriving (Eq,Show)

-- | Parse @midi-osc@ @/midi/@ message.
parse_b :: Integral n => OSC -> [n]
parse_b m =
    case m of
      Message "/midi" [Int _,Blob b] -> map fromIntegral (B.unpack b)
      _ -> []

-- | Variant of 'parse_b' that give status byte as low and high.
parse_c :: Integral n => OSC -> [n]
parse_c m =
    case parse_b m of
      st:dt -> let (l,h) = st `divMod` 16 in l:h:dt
      _ -> []

-- | Variant of 'parse_c' that constructs a 'Midi_Message'.
parse_m :: Integral n => OSC -> Midi_Message n
parse_m m =
    case parse_c m of
      [0x8,i,j,k] -> Note_Off i j k
      [0x9,i,j,0] -> Note_Off i j 0
      [0x9,i,j,k] -> Note_On i j k
      [0xA,i,j,k] -> Polyphic_Key_Pressure i j k
      [0xB,i,120,0] -> All_Sound_Off i
      [0xB,i,121,j] -> Reset_All_Controllers i j
      [0xB,i,122,j] -> Local_Control i j
      [0xB,i,123,0] -> All_Notes_Off i
      [0xB,i,124,0] -> Omni_Mode_Off i
      [0xB,i,125,0] -> Omni_Mode_On i
      [0xB,i,126,j] -> Mono_Mode_On i j
      [0xB,i,127,0] -> Poly_Mode_On i
      [0xB,i,j,k] -> Control_Change i j k
      [0xC,i,j] -> Program_Change i j
      [0xD,i,j] -> Chanel_Aftertouch i j
      [0xE,i,j] -> Pitch_Bend i j
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
midi_act :: Midi_Receiver IO Int -> OSC -> StateT (K Int) IO ()
midi_act f o = do
    let m = parse_m o
    n <- case m of
           Note_Off ch k _ -> k_get (ch,k)
           Note_On ch k _ -> k_alloc (ch,k)
           _ -> return (-1)
    liftIO (f m n)

-- | 'sequence_' '.' 'repeat'.
repeatM_ :: (Monad m) => m a -> m ()
repeatM_ = sequence_ . repeat

-- | Run midi system, handles 'E.AsyncException's.
start_midi :: (UDP -> Midi_Receiver IO Int) -> IO ()
start_midi receiver = do
  s_fd <- openUDP "127.0.0.1" 57110 -- midi-osc
  m_fd <- openUDP "127.0.0.1" 57150 -- midi-osc
  send m_fd (Message "/receive" [Int 0xffff])
  let step = liftIO (recv m_fd) >>=
             midi_act (receiver s_fd)
      ex e = print ("midi_run:",show (e::E.AsyncException)) >>
             close m_fd >>
             close s_fd
      runs = runStateT (repeatM_ step) (k_init 1000) >>
             return ()
  E.catch runs ex
  return ()
