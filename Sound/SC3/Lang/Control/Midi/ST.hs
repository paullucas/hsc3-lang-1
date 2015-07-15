-- | Maintain midi state, query functions.
module Sound.SC3.Lang.Control.Midi.ST where

import Control.Concurrent {- base -}
import qualified Data.Map as M {- containers -}

import Sound.SC3.Lang.Control.Midi {- hsc3-lang -}
import Sound.SC3.Lang.Control.Midi.OSC {- hsc3-lang -}

type Midi_7bit = Int
type Midi_Note = Midi_7bit
type Midi_Velocity = Midi_7bit
type Midi_Program = Midi_7bit
type Midi_CC_Ix = Midi_7bit
type Midi_CC_Value = Midi_7bit

type Midi_Key_Map = M.Map Midi_Note Midi_Velocity
type Midi_CC_Map = M.Map Midi_CC_Ix Midi_CC_Value

type Midi_State' = (Midi_Key_Map,Midi_Program,Midi_CC_Map)
type Midi_State = MVar Midi_State'

st_edit_km :: Midi_State -> (Midi_Note,Midi_Velocity) -> IO Midi_State
st_edit_km mv (k,v) =
    let f (m,p,c) = (if v == 0 then M.delete k m else M.insert k v m,p,c)
    in modifyMVar_ mv (return . f) >> return mv

st_edit_cc :: Midi_State -> (Midi_CC_Ix,Midi_CC_Value) -> IO Midi_State
st_edit_cc mv (k,v) =
    let f (m,p,c) = (m,p,M.insert k v c)
    in modifyMVar_ mv (return . f) >> return mv

st_edit_pc :: Midi_State -> Midi_Program -> IO Midi_State
st_edit_pc mv p =
    let f (m,_,c) = (m,p,c)
    in modifyMVar_ mv (return . f) >> return mv

p3_fst :: (t,u,v) -> t
p3_fst (t,_,_) = t

p3_third :: (t,u,v) -> v
p3_third (_,_,v) = v

st_read :: Midi_State -> IO Midi_State'
st_read = readMVar

st_access_km :: (Midi_Key_Map -> r) -> Midi_State -> IO r
st_access_km f mv = withMVar mv (return . f . p3_fst)

st_access_cc :: (Midi_CC_Map -> r) -> Midi_State -> IO r
st_access_cc f mv = withMVar mv (return . f . p3_third)

st_read_note :: Midi_State -> Midi_Note -> IO (Maybe Midi_Velocity)
st_read_note st k = st_access_km (M.lookup k) st

st_read_cc :: Midi_State -> Midi_CC_Ix -> IO Midi_CC_Value
st_read_cc st k = st_access_cc (M.findWithDefault 0 k) st

st_chord :: Midi_State -> IO [Midi_Note]
st_chord = st_access_km (map fst . M.toAscList)

st_init_f :: Midi_State -> Midi_Init_f Midi_State
st_init_f v _ = return v

st_recv_f :: Midi_Recv_f Midi_State
st_recv_f _ st msg =
    case msg of
      Note_Off _ j _ -> st_edit_km st (j,0)
      Note_On _ j k -> st_edit_km st (j,k)
      Control_Change _ j k -> st_edit_cc st (j,k)
      Program_Change _ j -> st_edit_pc st j
      _ -> print msg >> return st

st_run :: IO (Midi_State,ThreadId)
st_run = do
  v <- newMVar (M.empty,0,M.empty)
  th <- forkIO (run_midi (st_init_f v) st_recv_f)
  return (v,th)

{-
(st,th) <- st_run
st_chord st
readMVar st
st_read_note 55 st
st_read_cc 0 st
killThread th
-}
