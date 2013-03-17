-- | An 'Event' is a ('Key',/value/) map with associated meta data.
module Sound.SC3.Lang.Control.Event where

import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import GHC.Exts {- base -}
import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import System.Random {- base -}

import qualified Sound.SC3.Lang.Control.Duration as D
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P

-- | The type of the /key/ at an 'Event'.
type Key = String

data Field = F_Double {f_double :: Double}
           | F_Vector {f_vector :: [Field]}
           | F_String {f_string :: String}
           | F_Instr {f_instr :: I.Instrument}
             deriving (Eq,Show)

f_double' :: Field -> Maybe Double
f_double' f = case f of {F_Double n -> Just n;_ -> Nothing;}

f_double'' :: String -> Field -> Double
f_double'' err = fromMaybe (error ("f_double': " ++ err)) . f_double'

f_uop :: (Double -> Double) -> Field -> Field
f_uop f p =
    case p of
      F_Double n -> F_Double (f n)
      F_Vector v -> F_Vector (map (f_uop f) v)
      _ -> error "f_uop"

f_array :: [Double] -> Field
f_array = F_Vector . map F_Double

-- > f_binop (+) (F_Double 1) (F_Double 2) == F_Double 3
-- > f_binop (*) (f_array [1,2,3]) (f_array [3,4,5]) == f_array [3,8,15]
-- > f_binop (/) (F_Double 9) (F_Double 3) == F_Double 3
f_binop :: (Double -> Double -> Double) -> Field -> Field -> Field
f_binop f p q =
    case (p,q) of
      (F_Double m,F_Double n) -> F_Double (f m n)
      (F_Vector v,F_Vector w) -> F_Vector (zipWith (f_binop f) v w)
      _ -> error "f_binop"

instance IsString Field where
    fromString = F_String

instance Num Field where
    (+) = f_binop (+)
    (*) = f_binop (*)
    negate = f_uop negate
    abs = f_uop abs
    signum = f_uop signum
    fromInteger = F_Double . fromInteger

instance Fractional Field where
    recip = f_uop recip
    (/) = f_binop (/)
    fromRational n = F_Double (fromRational n)

instance Floating Field where
    pi = F_Double pi
    exp = f_uop exp
    log = f_uop log
    sqrt = f_uop sqrt
    (**) = f_binop (**)
    logBase = f_binop logBase
    sin = f_uop sin
    cos = f_uop cos
    tan = f_uop tan
    asin = f_uop asin
    acos = f_uop acos
    atan = f_uop atan
    sinh = f_uop sinh
    cosh = f_uop cosh
    tanh = f_uop tanh
    asinh = f_uop asinh
    acosh = f_uop acosh
    atanh = f_uop atanh

f_atf :: (Double -> a) -> Field -> a
f_atf f = f . f_double

f_atf2 :: (Double -> Double -> a) -> Field -> Field -> a
f_atf2 f p q =
    case (p,q) of
      (F_Double n1,F_Double n2) -> f n1 n2
      _ -> error "f_atf2: partial"

f_atf3 :: (Double -> Double -> Double -> a) -> Field -> Field -> Field -> a
f_atf3 f p q r =
    case (p,q,r) of
      (F_Double n1,F_Double n2,F_Double n3) -> f n1 n2 n3
      _ -> error "f_atf2: partial"

instance Real Field where
    toRational d =
        case d of
          F_Double n -> toRational n
          _ -> error "Field.toRational: partial"

instance RealFrac Field where
  properFraction d =
      let (i,j) = properFraction (f_double d)
      in (i,F_Double j)
  truncate = f_atf truncate
  round = f_atf round
  ceiling = f_atf ceiling
  floor = f_atf floor

instance RealFloat Field where
    floatRadix = f_atf floatRadix
    floatDigits = f_atf floatDigits
    floatRange = f_atf floatRange
    decodeFloat = f_atf decodeFloat
    encodeFloat i = F_Double . encodeFloat i
    exponent = f_atf exponent
    significand = f_uop significand
    scaleFloat i = f_uop (scaleFloat i)
    isNaN = f_atf isNaN
    isInfinite = f_atf isInfinite
    isDenormalized = f_atf isDenormalized
    isNegativeZero = f_atf isNegativeZero
    isIEEE = f_atf isIEEE
    atan2 = f_binop atan2

instance Ord Field where
    compare p q = case (p,q) of
                    (F_Double m,F_Double n) -> compare m n
                    _ -> error "Field.compare: partial"

instance Enum Field where
    fromEnum = f_atf fromEnum
    enumFrom = f_atf (map F_Double . enumFrom)
    enumFromThen = f_atf2 (\a -> map F_Double . enumFromThen a)
    enumFromTo = f_atf2 (\a -> map F_Double . enumFromTo a)
    enumFromThenTo = f_atf3 (\a b -> map F_Double . enumFromThenTo a b)
    toEnum = F_Double . fromIntegral

instance Random Field where
  randomR i g =
      case i of
        (F_Double l,F_Double r) ->
            let (n,g') = randomR (l,r) g
            in (F_Double n,g')
        _ -> error "Field.randomR: partial"
  random g = let (n,g') = randomR (0::Double,1::Double) g
             in (F_Double n,g')

instance UnaryOp Field
instance BinaryOp Field

type Event = M.Map Key Field

defaultEvent :: Event
defaultEvent = M.empty

e_union :: Event -> Event -> Event
e_union = M.union

-- | Lookup /k/ in /e/.
--
-- > e_get "k" defaultEvent == Nothing
e_get :: Key -> Event -> Maybe Field
e_get k = M.lookup k

e_get_double :: Key -> Event -> Maybe Double
e_get_double k = fmap (f_double'' k) . e_get k

e_get_array :: Key -> Event -> Maybe [Double]
e_get_array k = fmap (map (f_double'' k) . f_vector) . e_get k

type Type = String

-- > e_type defaultEvent == "s_new"
e_type :: Event -> Type
e_type = fromMaybe "s_new" . fmap f_string . e_get "type"

e_from_list :: [(Key,Field)] -> Event
e_from_list = M.fromList

-- > D.delta (e_duration defaultEvent) == 1
e_duration :: Event -> D.Duration Double
e_duration e =
    let f k = e_get_double k e
    in D.optDuration (f "tempo"
                     ,f "dur"
                     ,f "stretch"
                     ,f "legato"
                     ,f "sustain"
                     ,f "delta"
                     ,f "lag"
                     ,f "fwd'")

-- | The /sustain/ value of the duration model at /e/.
--
-- > sustain (e_from_list [("dur",1),("legato",0.5)]) == 0.5
sustain :: Event -> Double
sustain = D.sustain . e_duration

-- > P.midinote (e_pitch defaultEvent) == 60
e_pitch :: Event -> P.Pitch Double
e_pitch e =
    let f k = e_get_double k e
    in P.optPitch (f "mtranspose"
                  ,f "gtranspose"
                  ,f "ctranspose"
                  ,f "octave"
                  ,f "root"
                  ,f "degree"
                  ,e_get_array "scale" e
                  ,f "stepsPerOctave"
                  ,f "detune"
                  ,f "harmonic"
                  ,f "freq"
                  ,f "midinote"
                  ,f "note")

-- | The frequency of the 'pitch' of /e/.
--
-- > freq (e_from_list [("degree",5)]) == 440
-- > freq (e_from_list [("midinote",69)]) == 440
freq :: Event -> Double
freq = P.detunedFreq . e_pitch

e_id :: Event -> Maybe Double
e_id = e_get_double "id"

-- | Insert (/k/,/v/) into /e/.
--
-- > e_get "k" (e_insert "k" 1 defaultEvent) == Just 1
e_insert :: Key -> Field -> Event -> Event
e_insert k v = M.insert k v

-- | Lookup /db/ field of 'Event', the default value is @-20db@.
e_db :: Event -> Double
e_db = fromMaybe (-20) . e_get_double "db"

-- | Function to convert from decibels to linear amplitude.
dbAmp' :: Floating a => a -> a
dbAmp' a = 10 ** (a * 0.05)

-- | The linear amplitude of the amplitude model at /e/.
--
-- > e_amp (e_from_list [("db",-20)]) == 0.1
e_amp :: Event -> Double
e_amp e = fromMaybe (dbAmp' (e_db e)) (e_get_double "amp" e)

-- | The /fwd/ value of the duration model at /e/.
--
-- > e_fwd (e_from_list [("dur",1),("stretch",2)]) == 2
e_fwd :: Event -> Double
e_fwd = D.fwd . e_duration

-- | The /latency/ to compensate for when sending messages based on
-- the event.  Defaults to @0.1@.
latency :: Event -> Double
latency = fromMaybe 0.1 . e_get_double "latency"

-- | List of 'Key's used in pitch, duration and amplitude models.
--
-- > ("degree" `elem` model_keys) == True
model_keys :: [Key]
model_keys =
    ["amp","db"
    ,"delta","dur","legato","fwd'","stretch","sustain","tempo"
    ,"ctranspose","degree","freq","midinote","mtranspose","note","octave"
    ,"rest"]

-- | List of reserved 'Key's used in pitch, duration and amplitude
-- models.  These are keys that may be provided explicitly, but if not
-- will be calculated implicitly.
--
-- > ("freq" `elem` reserved) == True
reserved :: [Key]
reserved = ["freq","midinote","note"
           ,"delta","sustain"
           ,"amp"
           ,"instr","id","type"]

-- | Is 'Key' 'reserved'.
is_parameter :: (Key,a) -> Bool
is_parameter (k,_) = k `notElem` reserved

-- | Extract non-'reserved' 'Keys' from 'Event'.
parameters :: Event -> [(Key,Double)]
parameters =
    map (\(k,v) -> (k,f_double'' k v)) .
    filter is_parameter .
    M.toList

-- | 'Value' editor for 'Key' at 'Event', with default value in case
-- 'Key' is not present.
e_edit :: Key -> Field -> (Field -> Field) -> Event -> Event
e_edit k v f e =
    case e_get k e of
      Just n -> e_insert k (f n) e
      Nothing -> e_insert k (f v) e

-- | Variant of 'edit_v' with no default value.
e_edit' :: Key -> (Field -> Field) -> Event -> Event
e_edit' k f e =
    case e_get k e of
      Just n -> e_insert k (f n) e
      Nothing -> e

e_instr :: Event -> Maybe I.Instrument
e_instr = fmap f_instr . e_get "instr"

-- | Extract 'I.Instrument' name from 'Event', or @default@.
e_instr_name :: Event -> String
e_instr_name e =
    case e_instr e of
      Nothing -> "default"
      Just (I.InstrumentDef s _) -> synthdefName s
      Just (I.InstrumentName s _) -> s

-- | Extract 'I.Instrument' definition from 'Event' if present.
e_instr_def :: Event -> Maybe Synthdef
e_instr_def e =
    case e_instr e of
      Nothing -> Nothing
      Just (I.InstrumentDef s _) -> Just s
      Just (I.InstrumentName _ _) -> Nothing

-- | 'I.send_release' of 'I.Instrument' at 'Event'.
e_instr_send_release :: Event -> Bool
e_instr_send_release e =
    case e_instr e of
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

-- | Merge two time-stamped 'Event' sequences.  Note that this uses
-- 'fwd' to calculate start times.
e_merge' :: (Time,[Event]) -> (Time,[Event]) -> [(Time,Event)]
e_merge' (pt,p) (qt,q) =
    let p_st = map (+ pt) (0 : scanl1 (+) (map e_fwd p))
        q_st = map (+ qt) (0 : scanl1 (+) (map e_fwd q))
    in f_merge (zip p_st p) (zip q_st q)

-- | Insert /fwd/ 'Key's into a time-stamped 'Event' sequence.
e_add_fwd :: [(Time,Event)] -> [Event]
e_add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' ->
          e_insert "fwd'" (F_Double (t1 - t0)) e0 : e_add_fwd ((t1,e1):e')
      _ -> map snd e

-- | Composition of 'add_fwd' and 'merge''.
e_merge :: (Time,[Event]) -> (Time,[Event]) -> [Event]
e_merge p q = e_add_fwd (e_merge' p q)

-- | Does 'Event' have a non-zero @rest@ key.
is_rest :: Event -> Bool
is_rest e =
    case e_get_double "rest" e of
      Just r -> r > 0
      Nothing -> False

-- * SC3

-- | Generate @SC3@ 'Bundle' messages describing 'Event'.  Consults the
-- 'instrument_send_release' in relation to gate command.
to_sc3_bundle :: Time -> Int -> Event-> Maybe (Bundle,Bundle)
to_sc3_bundle t j e =
    let s = e_instr_name e
        sr = e_instr_send_release e
        p = e_pitch e
        d = e_duration e
        rt = D.sustain d {- rt = release time -}
        f = P.detunedFreq p
        pr = ("freq",f)
             : ("midinote",P.midinote p)
             : ("note",P.note p)
             : ("delta",D.delta d)
             : ("sustain",rt)
             : ("amp",e_amp e)
             : parameters e
        i = fromMaybe j (fmap floor (e_id e))
        t' = t + realToFrac (latency e)
    in if is_rest e || isNaN f
       then Nothing
       else let m_on = case e_type e of
                         "s_new" -> [s_new s i AddToTail 1 pr]
                         "n_set" -> [n_set i pr]
                         "rest" -> []
                         _ -> error "Event.type"
                m_off = if not sr
                        then []
                        else case e_type e of
                               "s_new" -> [n_set i [("gate",0)]]
                               "n_set" -> [n_set i [("gate",0)]]
                               "rest" -> []
                               _ -> error "Event.type"
            in Just (Bundle t' m_on
                    ,Bundle (t' + realToFrac rt) m_off)

-- | Send 'Event' to @scsynth@ at 'Transport'.
e_send :: Transport m => Time -> Int -> Event -> m ()
e_send t j e =
    let voidM a = a >> return ()
    in case to_sc3_bundle t j e of
        Just (p,q) -> do case e_instr_def e of
                           Just d -> voidM (async (d_recv d))
                           Nothing -> return ()
                         sendBundle p
                         sendBundle q
        Nothing -> return ()

-- | Function to audition a sequence of 'Event's using the @scsynth@
-- instance at 'Transport' starting at indicated 'Time'.
e_tplay :: Transport m => Time -> [Int] -> [Event] -> m ()
e_tplay t j e =
    case (j,e) of
      (_,[]) -> return ()
      ([],_) -> error "e_tplay: no-id"
      (i:j',d:e') -> do let t' = t + e_fwd d
                        e_send t i d
                        pauseThreadUntil t'
                        e_tplay t' j' e'

-- | Variant of 'e_tplay' with current clock time from 'time' as start
-- time.  This function is used to implement the pattern instances of
-- 'Audible'.
e_play :: Transport m => [Int] -> [Event] -> m ()
e_play lj le = do
  st <- time
  e_tplay st lj le
