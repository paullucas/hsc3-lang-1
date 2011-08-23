module Sound.SC3.Lang.Collection.Event where

import qualified Data.Map as M
import Data.Maybe
import Sound.SC3.ID
import Sound.SC3.Lang.Math.Duration
import Sound.SC3.Lang.Math.Pitch

type Key = String
type Value = Double
type Type = String
data Instrument = InstrumentDef Synthdef
                | InstrumentName String
data Event = Event {e_type :: Type
                   ,e_id :: Int
                   ,e_instrument :: Instrument
                   ,e_pitch :: Pitch Double
                   ,e_duration :: Duration Double
                   ,e_map :: M.Map Key Value}

e_noID :: Int
e_noID = -2

defaultEvent :: Event
defaultEvent =
    Event {e_type = "unknown"
          ,e_id = e_noID
          ,e_instrument = InstrumentName "default"
          ,e_pitch = defaultPitch
          ,e_duration = defaultDuration
          ,e_map = M.empty}

e_lookup :: Key -> Event -> Maybe Value
e_lookup k e = M.lookup k (e_map e)

e_lookup_v :: Value -> Key -> Event -> Value
e_lookup_v v k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> v'

e_lookup_f :: (Event -> Value) -> Key -> Event -> Value
e_lookup_f f k e =
    case e_lookup k e of
      Nothing -> f e
      Just v' -> v'

e_insert :: Key -> Value -> Event -> Event
e_insert k v e =
    case k of
      "mtranspose" -> e {e_pitch = (e_pitch e) {mtranspose = v}}
      "gtranspose" -> e {e_pitch = (e_pitch e) {gtranspose = v}}
      "octave" -> e {e_pitch = (e_pitch e) {octave = v}}
      "root" -> e {e_pitch = (e_pitch e) {root = v}}
      "degree" -> e {e_pitch = (e_pitch e) {degree = v}}
      "stepsPerOctave" -> e {e_pitch = (e_pitch e) {stepsPerOctave = v}}
      "detune" -> e {e_pitch = (e_pitch e) {detune = v}}
      "harmonic" -> e {e_pitch = (e_pitch e) {harmonic = v}}
      "note" -> e {e_pitch = (e_pitch e) {note_f = const v}}
      "midinote" -> e {e_pitch = (e_pitch e) {midinote_f = const v}}
      "freq" -> e {e_pitch = (e_pitch e) {freq_f = const v}}
      "dur" -> e {e_duration = (e_duration e) {dur = v}}
      "legato" -> e {e_duration = (e_duration e) {legato = v}}
      "fwd'" -> e {e_duration = (e_duration e) {fwd' = Just v}}
      _ -> e {e_map = M.insert k v (e_map e)}

e_insert_l :: [(Key,Value)] -> Event -> Event
e_insert_l l e =
    case l of
      [] -> e
      ((k,v):l') -> e_insert_l l' (e_insert k v e)

e_set_fwd :: Double -> Event -> Event
e_set_fwd n e = e {e_duration = (e_duration e) {fwd' = Just n}}

e_lookup_m :: Value -> (Value -> Value) -> Key -> Event -> Value
e_lookup_m v f k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> f v'

e_freq :: Event -> Double
e_freq = detunedFreq . e_pitch

e_dur :: Event -> Value
e_dur = e_lookup_v 1 "dur"

e_db :: Event -> Value
e_db = e_lookup_v (-20) "db"

dbAmp' :: Floating a => a -> a
dbAmp' a = 10 ** (a * 0.05)

e_amp :: Event -> Value
e_amp e = e_lookup_v (dbAmp' (e_db e)) "amp" e

e_fwd :: Event -> Double
e_fwd = fwd . e_duration

e_sustain :: Event -> Double
e_sustain = sustain . e_duration

e_reserved :: [Key]
e_reserved =
    ["amp","db"
    ,"dur","legato","fwd","sustain"
    ,"ctranspose","degree","freq","midinote","mtranspose","note","octave"]

e_arg' :: (Key,Value) -> Maybe (Key,Value)
e_arg' (k,v) =
    if k `elem` e_reserved
    then Nothing
    else Just (k,v)

e_arg :: Event -> [(Key,Value)]
e_arg = mapMaybe e_arg' . M.toList . e_map

e_edit_v :: Key -> Value -> (Value -> Value) -> Event -> Event
e_edit_v k v f e =
    case e_lookup k e of
      Just n -> e_insert k (f n) e
      Nothing -> e_insert k (f v) e

e_edit :: Key -> (Value -> Value) -> Event -> Event
e_edit k f e =
    case e_lookup k e of
      Just n -> e_insert k (f n) e
      Nothing -> e

e_from_list :: Type -> Int -> Instrument -> [(Key,Value)] -> Event
e_from_list t n i l =
    let e = defaultEvent {e_type = t
                         ,e_id = n
                         ,e_instrument = i}
    in e_insert_l l e

e_instrument_name :: Event -> String
e_instrument_name e =
    case e_instrument e of
      InstrumentDef s -> synthdefName s
      InstrumentName s -> s

e_instrument_def :: Event -> Maybe Synthdef
e_instrument_def e =
    case e_instrument e of
      InstrumentDef s -> Just s
      InstrumentName _ -> Nothing

{-
e_unions :: [Event] -> Event
e_unions e =
    let (t:_) = map e_type e
        (i:_) = map e_instrument e
    in Event t (-1) i (M.unions (map e_map e))
-}

defaultInstrument :: Synthdef
defaultInstrument =
    let f = control KR "freq" 440
        a = control KR "amp" 0.1
        p = control KR "pan" 0
        g = control KR "gate" 1
        e = linen g 0.01 0.7 0.3 RemoveSynth
        f3 = mce [f,f + rand 'a' (-0.4) 0,f + rand 'b' 0 0.4]
        l = xLine KR (rand 'c' 4000 5000) (rand 'd' 2500 3200) 1 DoNothing
        z = lpf (mix (varSaw AR f3 0 0.3 * 0.3)) l * e
    in synthdef "default" (out 0 (pan2 z p a))
