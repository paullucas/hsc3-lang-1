module Sound.SC3.Lang.Collection.Event where

import qualified Data.Map as M
import Data.Maybe
import GHC.Exts (IsString(..))
import Sound.SC3.Lang.Math.Pitch

type Key = String
type Type = String
data Event a = Event {e_type :: Type
                     ,e_id :: Int
                     ,e_map :: M.Map Key a}
               deriving (Show)

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
e_insert k v (Event t n m) = Event t n (M.insert k v m)

to_r :: Real a => a -> Double
to_r = fromRational . toRational

e_lookup_r :: Real a => a -> Key -> Event a -> Double
e_lookup_r v k = to_r . e_lookup_v v k

e_lookup_m :: b -> (a -> b) -> Key -> Event a -> b
e_lookup_m v f k e =
    case e_lookup k e of
      Nothing -> v
      Just v' -> f v'

e_pitch :: Real a => Event a -> Pitch Double
e_pitch e =
    let get_r v k = e_lookup_r v k e
        get_m v k = e_lookup_m v (const . to_r) k e
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

e_freq :: Real a => Event a -> Double
e_freq = detunedFreq . e_pitch

e_dur :: Num a => Event a -> a
e_dur = e_lookup_v 1 "dur"

e_dur' :: Real a => Event a -> Double
e_dur' = to_r . e_dur

e_fwd :: Num a => Event a -> a
e_fwd e = e_lookup_v (e_dur e) "fwd" e

e_fwd' :: Real a => Event a -> Double
e_fwd' = to_r . e_fwd

e_legato :: Fractional a => Event a -> a
e_legato = e_lookup_v 0.8 "legato"

e_legato' :: (Fractional a,Real a) => Event a -> Double
e_legato' = to_r . e_legato

e_stretch :: Num a => Event a -> a
e_stretch = e_lookup_v 1 "stretch"

e_stretch' :: Real a => Event a -> Double
e_stretch' = to_r . e_stretch

e_sustain :: Fractional a => Event a -> a
e_sustain =
    let f e = e_dur e * e_legato e * e_stretch e
    in e_lookup_f f "sustain"

e_sustain' :: (Real a,Fractional a) => Event a -> Double
e_sustain' = to_r . e_sustain

e_instrument :: IsString s => Event s -> s
e_instrument = e_lookup_v (fromString "default") "instrument"

--e_instrument' :: Event -> String
--e_instrument' = datum_str' . e_instrument

e_reserved :: [Key]
e_reserved = ["dur","legato","fwd","sustain"
             ,"freq","note","octave"]

e_arg' :: Real a => (Key,a) -> Maybe (Key,Double)
e_arg' (k,v) =
    if k `elem` e_reserved
    then Nothing
    else Just (k,to_r v)

e_arg :: Real a => Event a -> [(Key,Double)]
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

e_from_list :: Type -> Int -> [(Key,a)] -> Event a
e_from_list t n = Event t n . M.fromList

e_unions :: [Event a] -> Event a
e_unions e =
    let (t:_) = map e_type e
    in Event t (-1) (M.unions (map e_map e))
