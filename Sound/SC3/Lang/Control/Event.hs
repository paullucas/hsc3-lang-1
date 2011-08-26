module Sound.SC3.Lang.Control.Event where

import qualified Data.Map as M
import Data.Maybe
import qualified Sound.OpenSoundControl as O
import qualified Sound.SC3.Server as S
import qualified Sound.SC3.Lang.Control.Duration as D
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P

type Key = String
type Value = Double
type Type = String
data Event = Event {e_type :: Type
                   ,e_id :: Maybe Int
                   ,e_instrument :: Maybe I.Instrument
                   ,e_map :: M.Map Key Value}
                  deriving (Eq,Show)

defaultEvent :: Event
defaultEvent =
    Event {e_type = "unknown"
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.empty}

lookup_m :: Key -> Event -> Maybe Value
lookup_m k e = M.lookup k (e_map e)

lookup_v :: Value -> Key -> Event -> Value
lookup_v v k e =
    case lookup_m k e of
      Nothing -> v
      Just v' -> v'

lookup_t :: t -> (Value -> t) -> Key -> Event -> t
lookup_t v f k e =
    case lookup_m k e of
      Nothing -> v
      Just v' -> f v'

pitch :: Event -> P.Pitch Double
pitch e =
    let get_r v k = lookup_v v k e
        get_m v k = lookup_t v const k e
    in P.Pitch {P.mtranspose = get_r 0 "mtranspose"
               ,P.gtranspose = get_r 0 "gtranspose"
               ,P.ctranspose = get_r 0 "ctranspose"
               ,P.octave = get_r 5 "octave"
               ,P.root = get_r 0 "root"
               ,P.degree = get_r 0 "degree"
               ,P.scale = [0, 2, 4, 5, 7, 9, 11]
               ,P.stepsPerOctave = get_r 12 "stepsPerOctave"
               ,P.detune = get_r 0 "detune"
               ,P.harmonic = get_r 1 "harmonic"
               ,P.freq_f = get_m P.default_freq_f "freq"
               ,P.midinote_f = get_m P.default_midinote_f "midinote"
               ,P.note_f = get_m P.default_note_f "note"}

duration :: Event -> D.Duration Double
duration e =
    let get_r v k = lookup_v v k e
        get_m v k = lookup_t v const k e
        get_o k = lookup_m k e
    in D.Duration {D.tempo = get_r 60 "tempo"
                  ,D.dur = get_r 1 "dur"
                  ,D.stretch = get_r 1 "stretch"
                  ,D.legato = get_r 0.8 "legato"
                  ,D.sustain_f = get_m D.default_sustain_f "sustain"
                  ,D.delta_f = get_m D.default_delta_f "delta"
                  ,D.lag = get_r 0.1 "lag"
                  ,D.fwd' = get_o "fwd'"}

insert :: Key -> Value -> Event -> Event
insert k v e = e {e_map = M.insert k v (e_map e)}

freq :: Event -> Double
freq = P.detunedFreq . pitch

db :: Event -> Value
db = lookup_v (-20) "db"

dbAmp' :: Floating a => a -> a
dbAmp' a = 10 ** (a * 0.05)

amp :: Event -> Value
amp e = lookup_v (dbAmp' (db e)) "amp" e

fwd :: Event -> Double
fwd = D.fwd . duration

sustain :: Event -> Double
sustain = D.sustain . duration

reserved :: [Key]
reserved =
    ["amp","db"
    ,"delta","dur","legato","fwd'","stretch","sustain","tempo"
    ,"ctranspose","degree","freq","midinote","mtranspose","note","octave"]

parameters' :: (Key,Value) -> Maybe (Key,Value)
parameters' (k,v) =
    if k `elem` reserved
    then Nothing
    else Just (k,v)

parameters :: Event -> [(Key,Value)]
parameters = mapMaybe parameters' . M.toList . e_map

edit_v :: Key -> Value -> (Value -> Value) -> Event -> Event
edit_v k v f e =
    case lookup_m k e of
      Just n -> insert k (f n) e
      Nothing -> insert k (f v) e

edit :: Key -> (Value -> Value) -> Event -> Event
edit k f e =
    case lookup_m k e of
      Just n -> insert k (f n) e
      Nothing -> e

from_list :: Type -> Maybe Int -> Maybe I.Instrument -> [(Key,Value)] -> Event
from_list t n i l =
    Event {e_type = t
          ,e_id = n
          ,e_instrument = i
          ,e_map = M.fromList l}

event :: [(Key,Value)] -> Event
event l =
    Event {e_type = "s_new"
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.fromList l}

instrument_name :: Event -> String
instrument_name e =
    case e_instrument e of
      Nothing -> "default"
      Just (I.InstrumentDef s) -> S.synthdefName s
      Just (I.InstrumentName s) -> s

instrument_def :: Event -> Maybe S.Synthdef
instrument_def e =
    case e_instrument e of
      Nothing -> Nothing
      Just (I.InstrumentDef s) -> Just s
      Just (I.InstrumentName _) -> Nothing

f_merge :: Ord a => [(a,t)] -> [(a,t)] -> [(a,t)]
f_merge p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      ((t0,e0):r0,(t1,e1):r1) ->
            if t0 <= t1
            then (t0,e0) : f_merge r0 q
            else (t1,e1) : f_merge p r1

{-
f_merge (zip [0,2..10] ['a'..]) (zip [0,4..12] ['A'..])
-}

type Time = Double

-- note that this uses fwd to calculate start times.
merge' :: (Time,[Event]) -> (Time,[Event]) -> [(Time,Event)]
merge' (pt,p) (qt,q) =
    let p_st = map (+ pt) (0 : scanl1 (+) (map fwd p))
        q_st = map (+ qt) (0 : scanl1 (+) (map fwd q))
    in f_merge (zip p_st p) (zip q_st q)

add_fwd :: [(Time,Event)] -> [Event]
add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' ->
          insert "fwd'" (t1 - t0) e0 : add_fwd ((t1,e1):e')
      _ -> map snd e

merge :: (Time,[Event]) -> (Time,[Event]) -> [Event]
merge p q = add_fwd (merge' p q)

-- t = time, s = instrument name
-- rt = release time, pr = parameters
-- ty:_p suffix (p = persist) does not send gate
to_sc3_osc :: Double -> Int -> Event -> Maybe (O.OSC,O.OSC)
to_sc3_osc t j e =
    let s = instrument_name e
        rt = sustain e
        f = freq e
        a = amp e
        pr = ("freq",f) : ("amp",a) : parameters e
        i = case e_id e of
              Nothing -> j
              Just i' -> i'
    in if isNaN f
       then Nothing
       else let m_on = case e_type e of
                         "s_new" -> [S.s_new s i S.AddToTail 1 pr]
                         "s_new_p" -> [S.s_new s i S.AddToTail 1 pr]
                         "n_set" -> [S.n_set i pr]
                         "n_set_p" -> [S.n_set i pr]
                         "rest" -> []
                         _ -> error "to_sc3_osc:m_on:type"
                m_off = case e_type e of
                         "s_new" -> [S.n_set i [("gate",0)]]
                         "s_new_p" -> []
                         "n_set" -> [S.n_set i [("gate",0)]]
                         "n_set_p" -> []
                         "rest" -> []
                         _ -> error "to_sc3_osc:m_off:type"
            in Just (O.Bundle (O.UTCr t) m_on
                    ,O.Bundle (O.UTCr (t+rt)) m_off)
