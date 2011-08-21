module Sound.SC3.Lang.Collection.Event where

import qualified Data.Map as M
import Data.Maybe
import Sound.SC3.ID
import Sound.SC3.Lang.Math.Pitch

type Key = String
type Type = String
type Instrument = Either Synthdef String
data Event a = Event {e_type :: Type
                     ,e_id :: Int
                     ,e_instrument :: Instrument
                     ,e_map :: M.Map Key a}

e_lookup :: Key -> Event a -> Maybe a
e_lookup k e = M.lookup k (e_map e)

e_lookup_v :: a -> Key -> Event a -> a
e_lookup_v v k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> v'

e_lookup_f :: (Event a -> a) -> Key -> Event a -> a
e_lookup_f f k e =
    case e_lookup k e of
      Nothing -> f e
      Just v' -> v'

e_insert :: Key -> a -> Event a -> Event a
e_insert k v (Event t n i m) = Event t n i (M.insert k v m)

e_lookup_m :: b -> (a -> b) -> Key -> Event a -> b
e_lookup_m v f k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> f v'

e_pitch :: Event Double -> Pitch Double
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

e_freq :: Event Double -> Double
e_freq = detunedFreq . e_pitch

e_dur :: Num a => Event a -> a
e_dur = e_lookup_v 1 "dur"

e_fwd :: Num a => Event a -> a
e_fwd e = e_lookup_v (e_dur e) "fwd" e * e_stretch e

e_db :: Num a => Event a -> a
e_db = e_lookup_v (-20) "db"

dbAmp' :: Floating a => a -> a
dbAmp' a = 10 ** (a * 0.05)

e_amp :: Floating a => Event a -> a
e_amp e = e_lookup_v (dbAmp' (e_db e)) "amp" e

e_legato :: Fractional a => Event a -> a
e_legato = e_lookup_v 0.8 "legato"

e_stretch :: Num a => Event a -> a
e_stretch = e_lookup_v 1 "stretch"

e_sustain :: Fractional a => Event a -> a
e_sustain =
    let f e = e_dur e * e_legato e * e_stretch e
    in e_lookup_f f "sustain"

e_reserved :: [Key]
e_reserved =
    ["amp","db"
    ,"dur","legato","fwd","sustain"
    ,"ctranspose","degree","freq","midinote","mtranspose","note","octave"]

e_arg' :: (Key,a) -> Maybe (Key,a)
e_arg' (k,v) =
    if k `elem` e_reserved
    then Nothing
    else Just (k,v)

e_arg :: Event a -> [(Key,a)]
e_arg = mapMaybe e_arg' . M.toList . e_map

e_edit_v :: Key -> a -> (a -> a) -> Event a -> Event a
e_edit_v k v f e =
    case e_lookup k e of
      Just n -> e_insert k (f n) e
      Nothing -> e_insert k (f v) e

e_edit :: Key -> (a -> a) -> Event a -> Event a
e_edit k f e =
    case e_lookup k e of
      Just n -> e_insert k (f n) e
      Nothing -> e

e_to_list :: Event a -> [(Key,a)]
e_to_list = M.toList . e_map

e_from_list :: Type -> Int -> Instrument -> [(Key,a)] -> Event a
e_from_list t n i = Event t n i . M.fromList


e_instrument_name :: Event a -> String
e_instrument_name e =
    case e_instrument e of
      Left s -> synthdefName s
      Right s -> s

e_instrument_def :: Event a -> Maybe Synthdef
e_instrument_def e =
    case e_instrument e of
      Left s -> Just s
      Right _ -> Nothing

e_unions :: [Event a] -> Event a
e_unions e =
    let (t:_) = map e_type e
        (i:_) = map e_instrument e
    in Event t (-1) i (M.unions (map e_map e))

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
