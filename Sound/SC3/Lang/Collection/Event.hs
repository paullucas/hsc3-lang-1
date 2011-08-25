module Sound.SC3.Lang.Collection.Event
    (Event(..),Type,Key,Value,E_Time,defaultEvent
    ,Instrument(..),defaultInstrument
    ,event,e_insert,e_edit,e_edit_v,e_from_list,e_merge
    ,e_instrument_name,e_instrument_def
    ,e_fwd,e_sustain,e_freq,e_amp
    ,e_parameters,e_sc3_osc) where

import qualified Data.Map as M
import Data.Maybe
import Sound.OpenSoundControl
import Sound.SC3.ID
import Sound.SC3.Lang.Math.Duration as D
import Sound.SC3.Lang.Math.Pitch

type Key = String
type Value = Double
type Type = String
data Instrument = InstrumentDef Synthdef
                | InstrumentName String
data Event = Event {e_type :: Type
                   ,e_id :: Maybe Int
                   ,e_instrument :: Maybe Instrument
                   ,e_map :: M.Map Key Value}

{-
e_noID :: Int
e_noID = -2
-}

defaultEvent :: Event
defaultEvent =
    Event {e_type = "unknown"
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.empty}

e_lookup :: Key -> Event -> Maybe Value
e_lookup k e = M.lookup k (e_map e)

e_lookup_v :: Value -> Key -> Event -> Value
e_lookup_v v k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> v'

{-
e_lookup_f :: (Event -> Value) -> Key -> Event -> Value
e_lookup_f f k e =
    case e_lookup k e of
      Nothing -> f e
      Just v' -> v'
-}

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

{-
e_insert_l :: [(Key,Value)] -> Event -> Event
e_insert_l l e =
    case l of
      [] -> e
      ((k,v):l') -> e_insert_l l' (e_insert k v e)
-}

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

e_parameters' :: (Key,Value) -> Maybe (Key,Value)
e_parameters' (k,v) =
    if k `elem` e_reserved
    then Nothing
    else Just (k,v)

e_parameters :: Event -> [(Key,Value)]
e_parameters = mapMaybe e_parameters' . M.toList . e_map

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

e_from_list :: Type -> Maybe Int -> Maybe Instrument -> [(Key,Value)] -> Event
e_from_list t n i l =
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

e_instrument_name :: Event -> String
e_instrument_name e =
    case e_instrument e of
      Nothing -> "default"
      Just (InstrumentDef s) -> synthdefName s
      Just (InstrumentName s) -> s

e_instrument_def :: Event -> Maybe Synthdef
e_instrument_def e =
    case e_instrument e of
      Nothing -> Nothing
      Just (InstrumentDef s) -> Just s
      Just (InstrumentName _) -> Nothing

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

type E_Time = Double

-- note that this uses e_fwd to calculate start times.
e_merge' :: (E_Time,[Event]) -> (E_Time,[Event]) -> [(E_Time,Event)]
e_merge' (pt,p) (qt,q) =
    let p_st = map (+ pt) (0 : scanl1 (+) (map e_fwd p))
        q_st = map (+ qt) (0 : scanl1 (+) (map e_fwd q))
    in f_merge (zip p_st p) (zip q_st q)

add_fwd :: [(E_Time,Event)] -> [Event]
add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' ->
          e_insert "fwd'" (t1 - t0) e0 : add_fwd ((t1,e1):e')
      _ -> map snd e

e_merge :: (E_Time,[Event]) -> (E_Time,[Event]) -> [Event]
e_merge p q = add_fwd (e_merge' p q)

-- t = time, s = instrument name
-- rt = release time, pr = parameters
-- ty:_p suffix (p = persist) does not send gate
e_sc3_osc :: Double -> Int -> Event -> Maybe (OSC,OSC)
e_sc3_osc t j e =
    let s = e_instrument_name e
        rt = e_sustain e
        f = e_freq e
        a = e_amp e
        pr = ("freq",f) : ("amp",a) : e_parameters e
        i = case e_id e of
              Nothing -> j
              Just i' -> i'
    in if isNaN f
       then Nothing
       else let m_on = case e_type e of
                         "s_new" -> [s_new s i AddToTail 1 pr]
                         "s_new_p" -> [s_new s i AddToTail 1 pr]
                         "n_set" -> [n_set i pr]
                         "n_set_p" -> [n_set i pr]
                         "rest" -> []
                         _ -> error "e_osc:m_on:type"
                m_off = case e_type e of
                         "s_new" -> [n_set i [("gate",0)]]
                         "s_new_p" -> []
                         "n_set" -> [n_set i [("gate",0)]]
                         "n_set_p" -> []
                         "rest" -> []
                         _ -> error "e_osc:m_off:type"
            in Just (Bundle (UTCr t) m_on,Bundle (UTCr (t+rt)) m_off)
