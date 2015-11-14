-- | Trivial midi functions.
module Sound.SC3.Lang.Control.Midi.OSC where

import Control.Exception {- base -}
import Data.Bits {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Maybe {- base -}
import Sound.OSC.FD {- hosc -}

import Sound.SC3.Lang.Control.Midi {- hsc3-lang -}

-- * OSC

-- | Parse @midi-osc@ @/midi/@ message.
parse_b :: Integral n => Message -> [n]
parse_b m =
    case m of
      Message "/midi" [Int32 _,Midi (MIDI p q r s)] -> map fromIntegral [p,q,r,s]
      Message "/midi" [Int32 _,Blob b] -> map fromIntegral (B.unpack b)
      _ -> []

-- | Variant of 'parse_b' that give status byte as low and high.
parse_c :: Integral n => Message -> [n]
parse_c = status_sep . parse_b

-- | Variant of 'parse_c' that constructs a 'Midi_Message'.
parse_m :: (Bits n,Integral n) => Message -> Midi_Message n
parse_m = m_decode . parse_c

-- | 'B.pack' of 'm_encode'.
m_pack :: (Bits a,Integral a) => Midi_Message a -> B.ByteString
m_pack = B.pack . map fromIntegral . m_encode

-- | Inverse of 'parse_m'.
m_message :: (Bits a,Integral a) => Int -> Midi_Message a -> Message
m_message i m = Message "/midi" [int32 i,Blob (m_pack m)]

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
