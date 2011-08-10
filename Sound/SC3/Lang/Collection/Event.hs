module Sound.SC3.Lang.Collection.Event where

import Sound.OpenSoundControl
import Sound.SC3.Lang.Math.Datum
import Sound.SC3.Lang.Math.Pitch

type Key = String
type Event = [(Key,Datum)]

e_lookup :: Key -> Event -> Maybe Datum
e_lookup = lookup

e_lookup_v :: Datum -> Key -> Event -> Datum
e_lookup_v v k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> v'

e_lookup_f :: (Event -> Datum) -> Key -> Event -> Datum
e_lookup_f f k e =
    case e_lookup k e of
      Nothing -> f e
      Just v' -> v'

e_lookup_r :: Double -> Key -> Event -> Double
e_lookup_r v k = datum_r' . e_lookup_v (Double v) k

e_lookup_m :: a -> (Datum -> a) -> Key -> Event -> a
e_lookup_m v f k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> f v'

e_pitch :: Event -> Pitch Double
e_pitch e =
    let get_r v k = e_lookup_r v k e
        get_m v k = e_lookup_m v (const . datum_r') k e
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

e_freq :: Event -> Double
e_freq = freq . e_pitch

e_dur :: Event -> Datum
e_dur = e_lookup_v (Double 1) "dur"

e_dur' :: Event -> Double
e_dur' = datum_r' . e_dur

e_legato :: Event -> Datum
e_legato = e_lookup_v (Double 0.8) "legato"

e_legato' :: Event -> Double
e_legato' = datum_r' . e_legato

e_stretch :: Event -> Datum
e_stretch = e_lookup_v (Double 1) "stretch"

e_stretch' :: Event -> Double
e_stretch' = datum_r' . e_stretch

e_sustain :: Event -> Datum
e_sustain =
    let f e = e_dur e * e_legato e * e_stretch e
    in e_lookup_f f "sustain"

e_sustain' :: Event -> Double
e_sustain' = datum_r' . e_sustain

e_instrument :: Event -> Datum
e_instrument = e_lookup_v (String "default") "instrument"

e_instrument' :: Event -> String
e_instrument' = datum_str' . e_instrument

e_reserved :: [String]
e_reserved = ["dur","legato","instrument"]

e_arg :: (String,Datum) -> Maybe (String,Double)
e_arg (k,v) =
    case datum_r v of
      Nothing -> Nothing
      Just v' -> if k `elem` e_reserved
                 then Nothing
                 else Just (k,v')

