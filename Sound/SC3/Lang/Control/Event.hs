-- | An 'Event' is a ('Key','Value') map.
module Sound.SC3.Lang.Control.Event where

import qualified Data.Map as M
import Data.Maybe
import qualified Sound.OpenSoundControl as O
import qualified Sound.SC3.Server as S
import qualified Sound.SC3.Lang.Control.Duration as D
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P

-- | The type of the /key/ at an 'Event'.
type Key = String

-- | The type of the /value/ at an 'Event'.
type Value = Double

-- | The /type/ of an 'Event'.
type Type = String

-- | An 'Event' has a 'Type', possibly an integer identifier, possibly
-- an 'I.Instrument' and a map of ('Key','Value') pairs.
data Event = Event {e_type :: Type
                   ,e_id :: Maybe Int
                   ,e_instrument :: Maybe I.Instrument
                   ,e_map :: M.Map Key Value}
                  deriving (Eq,Show)

-- | The /default/ empty event.
defaultEvent :: Event
defaultEvent =
    Event {e_type = "unknown"
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.empty}

-- | Lookup /k/ in /e/.
--
-- > lookup_m "k" defaultEvent == Nothing
lookup_m :: Key -> Event -> Maybe Value
lookup_m k e = M.lookup k (e_map e)

-- | Variant of 'lookup_m' with a default value /v/.
--
-- > lookup_v 1 "k" defaultEvent == 1
lookup_v :: Value -> Key -> Event -> Value
lookup_v v k e = fromMaybe v (lookup_m k e)

-- | Variant of 'lookup_v' with a transformation function.
--
-- > lookup_t 1 negate "k" defaultEvent == 1
-- > lookup_t 1 negate "k" (insert "k" 1 defaultEvent) == -1
lookup_t :: t -> (Value -> t) -> Key -> Event -> t
lookup_t v f k e =
    case lookup_m k e of
      Nothing -> v
      Just v' -> f v'

-- | Lookup 'Pitch' model parameters at /e/ and construct a 'Pitch'
-- value.
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

-- | Lookup 'D.Duration' model parameters at an 'Event' and construct a
-- 'D.Duration' value.
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

-- | Insert (/k/,/v/) into /e/.
--
-- > lookup_m "k" (insert "k" 1 defaultEvent) == Just 1
insert :: Key -> Value -> Event -> Event
insert k v e = e {e_map = M.insert k v (e_map e)}

-- | The frequency of the 'pitch' of /e/.
--
-- > freq (event [("degree",5)]) == 440
-- > freq (event [("midinote",69)]) == 440
freq :: Event -> Double
freq = P.detunedFreq . pitch

-- | Lookup /db/ field of 'Event', the default value is @-20db@.
db :: Event -> Value
db = lookup_v (-20) "db"

-- | Function to convert from decibels to linear amplitude.
dbAmp' :: Floating a => a -> a
dbAmp' a = 10 ** (a * 0.05)

-- | The linear amplitude of the amplitude model at /e/.
--
-- > amp (event [("db",-20)]) == 0.1
amp :: Event -> Value
amp e = lookup_v (dbAmp' (db e)) "amp" e

-- | The /fwd/ value of the duration model at /e/.
--
-- > fwd (event [("dur",1),("stretch",2)]) == 2
fwd :: Event -> Double
fwd = D.fwd . duration

-- | The /sustain/ value of the duration model at /e/.
--
-- > sustain (event [("dur",1),("legato",0.5)]) == 0.5
sustain :: Event -> Double
sustain = D.sustain . duration

-- | The /latency/ to compensate for when sending messages based on
-- the event.  Defaults to @0.1@.
latency :: Event -> Double
latency = lookup_v 0.1 "latency"

-- | List of reserved /keys/ for pitch, duration and amplitude models.
--
-- > ("degree" `elem` reserved) == True
reserved :: [Key]
reserved =
    ["amp","db"
    ,"delta","dur","legato","fwd'","stretch","sustain","tempo"
    ,"ctranspose","degree","freq","midinote","mtranspose","note","octave"
    ,"rest"]

-- | If 'Key' is 'reserved' then 'Nothing', else 'id'.
parameters' :: (Key,Value) -> Maybe (Key,Value)
parameters' (k,v) =
    if k `elem` reserved
    then Nothing
    else Just (k,v)

-- | Extract non-'reserved' 'Keys' from 'Event'.
parameters :: Event -> [(Key,Value)]
parameters = mapMaybe parameters' . M.toList . e_map

-- | 'Value' editor for 'Key' at 'Event', with default value in case
-- 'Key' is not present.
edit_v :: Key -> Value -> (Value -> Value) -> Event -> Event
edit_v k v f e =
    case lookup_m k e of
      Just n -> insert k (f n) e
      Nothing -> insert k (f v) e

-- | Variant of 'edit_v' with no default value.
edit :: Key -> (Value -> Value) -> Event -> Event
edit k f e =
    case lookup_m k e of
      Just n -> insert k (f n) e
      Nothing -> e

-- | Basic 'Event' constructor function with 'e_map' given as a list.
from_list :: Type -> Maybe Int -> Maybe I.Instrument -> [(Key,Value)] -> Event
from_list t n i l =
    Event {e_type = t
          ,e_id = n
          ,e_instrument = i
          ,e_map = M.fromList l}

-- | Construct an 'Event' from a list of (/key/,/value/) pairs.
--
-- > lookup_m "k" (event [("k",1)]) == Just 1
event :: [(Key,Value)] -> Event
event l =
    Event {e_type = "s_new"
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.fromList l}

-- | Extract 'I.Instrument' name from 'Event', or @default@.
instrument_name :: Event -> String
instrument_name e =
    case e_instrument e of
      Nothing -> "default"
      Just (I.InstrumentDef s _) -> S.synthdefName s
      Just (I.InstrumentName s _) -> s

-- | Extract 'I.Instrument' definition from 'Event' if present.
instrument_def :: Event -> Maybe S.Synthdef
instrument_def e =
    case e_instrument e of
      Nothing -> Nothing
      Just (I.InstrumentDef s _) -> Just s
      Just (I.InstrumentName _ _) -> Nothing

-- | 'I.send_release' of 'I.Instrument' at 'Event'.
instrument_send_release :: Event -> Bool
instrument_send_release e =
    case e_instrument e of
      Nothing -> True
      Just i -> I.send_release i

-- | Merge two sorted sequence of (/location/,/value/) pairs.
--
-- > let m = f_merge (zip [0,2..6] ['a'..]) (zip [0,3,6] ['A'..])
-- > in m == [(0,'a'),(0,'A'),(2,'b'),(3,'B'),(4,'c'),(6,'d'),(6,'C')]
f_merge :: Ord a => [(a,t)] -> [(a,t)] -> [(a,t)]
f_merge p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      ((t0,e0):r0,(t1,e1):r1) ->
            if t0 <= t1
            then (t0,e0) : f_merge r0 q
            else (t1,e1) : f_merge p r1

-- | Times are real valued @UTC@.
type Time = Double

-- | Merge two time-stamped 'Event' sequences.  Note that this uses
-- 'fwd' to calculate start times.
merge' :: (Time,[Event]) -> (Time,[Event]) -> [(Time,Event)]
merge' (pt,p) (qt,q) =
    let p_st = map (+ pt) (0 : scanl1 (+) (map fwd p))
        q_st = map (+ qt) (0 : scanl1 (+) (map fwd q))
    in f_merge (zip p_st p) (zip q_st q)

-- | Insert /fwd/ 'Key's into a time-stamped 'Event' sequence.
add_fwd :: [(Time,Event)] -> [Event]
add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' ->
          insert "fwd'" (t1 - t0) e0 : add_fwd ((t1,e1):e')
      _ -> map snd e

-- | Composition of 'add_fwd' and 'merge''.
merge :: (Time,[Event]) -> (Time,[Event]) -> [Event]
merge p q = add_fwd (merge' p q)

-- | Does 'Event' have a non-zero @rest@ key.
is_rest :: Event -> Bool
is_rest e =
    case lookup_m "rest" e of
      Just r -> r > 0
      Nothing -> False

-- | Generate @SC3@ 'O.OSC' messages describing 'Event'.  Consults the
-- 'instrument_send_release' to in relation to gate command.
to_sc3_osc :: Time -> Int -> Event -> Maybe (O.OSC,O.OSC)
to_sc3_osc t j e =
    let s = instrument_name e
        sr = instrument_send_release e
        rt = sustain e {- rt = release time -}
        f = freq e
        pr = ("freq",f) : ("amp",amp e) : ("sustain",rt) : parameters e
        i = fromMaybe j (e_id e)
        t' = t + latency e
    in if is_rest e || isNaN f
       then Nothing
       else let m_on = case e_type e of
                         "s_new" -> [S.s_new s i S.AddToTail 1 pr]
                         "n_set" -> [S.n_set i pr]
                         "rest" -> []
                         _ -> error "to_sc3_osc:m_on:type"
                m_off = case (e_type e,sr) of
                          ("s_new",True) -> [S.n_set i [("gate",0)]]
                          ("n_set",True) -> [S.n_set i [("gate",0)]]
                          _ -> []
            in Just (O.Bundle (O.UTCr t') m_on
                    ,O.Bundle (O.UTCr (t' + rt)) m_off)
