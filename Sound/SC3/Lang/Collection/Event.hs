module Sound.SC3.Lang.Collection.Event where

import qualified Data.Map as M
import Data.Maybe
import Sound.SC3.ID
import Sound.SC3.Lang.Math.Duration as D
import Sound.SC3.Lang.Math.Pitch

type Key = String
type Value = Double
type Type = String
data Instrument = InstrumentDef Synthdef
                | InstrumentName String
data Event = Event {e_type :: Type
                   ,e_id :: Int
                   ,e_instrument :: Instrument
                   ,e_map :: M.Map Key Value}

e_noID :: Int
e_noID = -2

defaultEvent :: Event
defaultEvent =
    Event {e_type = "unknown"
          ,e_id = e_noID
          ,e_instrument = InstrumentName "default"
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

e_lookup_m :: t -> (Value -> t) -> Key -> Event -> t
e_lookup_m v f k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> f v'

e_pitch :: Event -> Pitch Double
e_pitch e =
    let get_r v k = e_lookup_v v k e
        get_m v k = e_lookup_m v const k e
    in Pitch {mtranspose = get_r 0 "mtranspose"
             ,gtranspose = get_r 0 "gtranspose"
             ,ctranspose = get_r 0 "ctranspose"
             ,octave = get_r 5 "octave"
             ,root = get_r 0 "root"
             ,degree = get_r 0 "degree"
             ,scale = [0, 2, 4, 5, 7, 9, 11]
             ,stepsPerOctave = get_r 12 "stepsPerOctave"
             ,detune = get_r 0 "detune"
             ,harmonic = get_r 1 "harmonic"
             ,freq_f = get_m default_freq_f "freq"
             ,midinote_f = get_m default_midinote_f "midinote"
             ,note_f = get_m default_note_f "note"}

e_duration :: Event -> Duration Double
e_duration e =
    let get_r v k = e_lookup_v v k e
        get_m v k = e_lookup_m v const k e
        get_o k = e_lookup k e
    in Duration {tempo = get_r 60 "tempo"
                ,dur = get_r 1 "dur"
                ,stretch = get_r 1 "stretch"
                ,legato = get_r 0.8 "legato"
                ,sustain_f = get_m default_sustain_f "sustain"
                ,delta_f = get_m default_delta_f "delta"
                ,D.lag = get_r 0.1 "lag"
                ,fwd' = get_o "fwd'"}

e_insert :: Key -> Value -> Event -> Event
e_insert k v e = e {e_map = M.insert k v (e_map e)}

e_insert_l :: [(Key,Value)] -> Event -> Event
e_insert_l l e =
    case l of
      [] -> e
      ((k,v):l') -> e_insert_l l' (e_insert k v e)

e_freq :: Event -> Double
e_freq = detunedFreq . e_pitch

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
    ,"delta","dur","legato","fwd'","stretch","sustain","tempo"
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
