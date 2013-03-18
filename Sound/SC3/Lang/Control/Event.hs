-- | An 'Event' is a ('Key','Field') map.
module Sound.SC3.Lang.Control.Event where

import Data.List {- base -}
import qualified Data.Map as Map {- containers -}
import Data.Maybe {- base -}
import GHC.Exts {- base -}
import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import System.Random {- base -}

import qualified Sound.SC3.Lang.Control.Duration as D
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P
import qualified Sound.SC3.Lang.Math as M

-- * Field

data Field = F_Double {f_double :: Double}
           | F_Vector {f_vector :: [Field]}
           | F_String {f_string :: String}
           | F_Instr {f_instr :: I.Instr}
             deriving (Eq,Show)

-- | Maybe variant of 'f_double'.
f_double_m :: Field -> Maybe Double
f_double_m f = case f of {F_Double n -> Just n;_ -> Nothing;}

-- | Variant of 'f_double' with specified error message.
f_double_err :: String -> Field -> Double
f_double_err err = fromMaybe (error ("f_double: " ++ err)) . f_double_m

-- | Run 'round' at 'f_double'.
f_int_err :: String -> Field -> Int
f_int_err err = round . fromMaybe (error ("f_int: " ++ err)) . f_double_m

-- | Uniform vector constructor.
--
-- > f_array [1,2] == F_Vector [F_Double 1,F_Double 2]
f_array :: [Double] -> Field
f_array = F_Vector . map F_Double

-- | Numerical unary operator.
--
-- > f_uop negate (F_Double 1) == F_Double (-1)
f_uop :: (Double -> Double) -> Field -> Field
f_uop f p =
    case p of
      F_Double n -> F_Double (f n)
      F_Vector v -> F_Vector (map (f_uop f) v)
      _ -> error ("f_uop: " ++ show p)

-- | Numerical binary operator.
--
-- > f_binop (+) (F_Double 1) (F_Double 2) == F_Double 3
-- > f_binop (*) (f_array [1,2,3]) (f_array [3,4,5]) == f_array [3,8,15]
-- > f_binop (/) (F_Double 9) (F_Double 3) == F_Double 3
f_binop :: (Double -> Double -> Double) -> Field -> Field -> Field
f_binop f p q =
    case (p,q) of
      (F_Double m,F_Double n) -> F_Double (f m n)
      (F_Vector v,F_Vector w) -> F_Vector (zipWith (f_binop f) v w)
      _ -> error ("f_binop: " ++ show (p,q))

f_atf :: (Double -> a) -> Field -> a
f_atf f = f . f_double

f_atf2 :: (Double -> Double -> a) -> Field -> Field -> a
f_atf2 f p q =
    case (p,q) of
      (F_Double n1,F_Double n2) -> f n1 n2
      _ -> error ("f_atf2: " ++ show (p,q))

f_atf3 :: (Double -> Double -> Double -> a) -> Field -> Field -> Field -> a
f_atf3 f p q r =
    case (p,q,r) of
      (F_Double n1,F_Double n2,F_Double n3) -> f n1 n2 n3
      _ -> error ("f_atf3: " ++ show (p,q,r))

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

instance Real Field where
    toRational d =
        case d of
          F_Double n -> toRational n
          _ -> error ("Field.toRational: " ++ show d)

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
    compare p q =
        case (p,q) of
          (F_Double m,F_Double n) -> compare m n
          _ -> error ("Field.compare: " ++ show (p,q))

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
        _ -> error ("Field.randomR: " ++ show i)
  random g = let (n,g') = randomR (0::Double,1::Double) g
             in (F_Double n,g')

instance EqE Field where
instance OrdE Field where
instance UnaryOp Field
instance BinaryOp Field

-- * Key

-- | The type of the /key/ at an 'Event'.
type Key = String

-- | List of reserved 'Key's used in pitch, duration and amplitude
-- models.  These are keys that may be provided explicitly, but if not
-- will be calculated implicitly.
--
-- > ("freq" `elem` k_reserved) == True
k_reserved :: [Key]
k_reserved = ["freq","midinote","note"
             ,"delta","sustain"
             ,"amp"
             ,"instr","id","type","latency","rest"]

-- | Is 'Key' /not/ 'k_reserved'.
k_is_parameter :: (Key,a) -> Bool
k_is_parameter (k,_) = k `notElem` k_reserved

-- | List of 'Key's used in pitch, duration and amplitude models.
k_models :: T3 [Key]
k_models =
    (["amp","db"]
    ,["delta","dur","legato","fwd'","stretch","sustain","tempo"]
    ,["ctranspose","degree","freq","midinote","mtranspose","note","octave"])

-- * Event

-- | An 'Event' is a ('Key','Field') map.
type Event = Map.Map Key Field

-- | Empty event.
e_empty :: Event
e_empty = Map.empty

-- | Insert (/k/,/v/) into /e/.
--
-- > e_get "k" (e_insert "k" 1 e_empty) == Just 1
e_insert :: Key -> Field -> Event -> Event
e_insert k v = Map.insert k v

-- | Event from association list.
--
-- > e_get "k" (e_from_list [("k",1)]) == Just 1
e_from_list :: [(Key,Field)] -> Event
e_from_list = Map.fromList

-- | Union of two events (left-biased).
--
-- > e_from_list [("a",0)] `e_union` e_from_list [("b",1)]
e_union :: Event -> Event -> Event
e_union = Map.union

-- | Lookup /k/ in /e/.
--
-- > e_get "k" e_empty == Nothing
e_get :: Key -> Event -> Maybe Field
e_get k = Map.lookup k

e_get_double :: Key -> Event -> Maybe Double
e_get_double k = fmap (f_double_err k) . e_get k

e_get_int :: Key -> Event -> Maybe Int
e_get_int k = fmap (f_int_err k) . e_get k

e_get_array :: Key -> Event -> Maybe [Double]
e_get_array k = fmap (map (f_double_err k) . f_vector) . e_get k

type Type = String

-- | 'Event' /type/.
--
-- > e_type e_empty == "s_new"
e_type :: Event -> Type
e_type = fromMaybe "s_new" . fmap f_string . e_get "type"

-- | Match on event types, in sequence: s_new, n_set, rest.
e_type_match :: Event -> T3 (Event -> t) -> t
e_type_match e (f,g,h) =
    case e_type e of
      "s_new" -> f e
      "n_set" -> g e
      "rest" -> h e
      _ -> error ("Event.type: " ++ show e)

-- | 'const' variant of 'e_type_match'.
e_type_match' :: Event -> T3 t -> t
e_type_match' e (f,g,h) = e_type_match e (const f,const g,const h)

-- | Generate 'D.Dur' from 'Event'.
--
-- > D.delta (e_duration e_empty) == 1
-- > D.fwd (e_duration (e_from_list [("dur",1),("stretch",2)])) == 2
--
-- > let e = e_from_list [("dur",1),("legato",0.5)]
-- > in D.occ (e_duration e) == 0.5
e_dur :: Event -> D.Dur
e_dur e =
    let f k = e_get_double k e
    in D.optDur (f "tempo"
                ,f "dur"
                ,f "stretch"
                ,f "legato"
                ,f "sustain"
                ,f "delta"
                ,f "lag"
                ,f "fwd'")

-- | Generate 'Pitch' from 'Event'.
--
-- > P.midinote (e_pitch e_empty) == 60
-- > P.freq (e_pitch (e_from_list [("degree",5)])) == 440
-- > P.midinote (e_pitch (e_from_list [("degree",5),("scale",f_array [0,2,3,5,7,8,10])])) == 68
-- > P.freq (e_pitch (e_from_list [("midinote",69)])) == 440
e_pitch :: Event -> P.Pitch
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

-- | 'Event' identifier.
e_id :: Event -> Maybe Int
e_id = e_get_int "id"

-- | Lookup /db/ field of 'Event'.
--
-- > e_db e_empty == (-20)
e_db :: Event -> Double
e_db = fromMaybe (-20) . e_get_double "db"

-- | The linear amplitude of the amplitude model at /e/.
--
-- > e_amp (e_from_list [("db",-60)]) == 0.001
-- > e_amp (e_from_list [("amp",0.01)]) == 0.01
-- > e_amp e_empty == 0.1
e_amp :: Event -> Double
e_amp e = fromMaybe (M.dbToRms (e_db e)) (e_get_double "amp" e)

-- | Message /latency/ of event.
--
-- > e_latency e_empty == 0.1
e_latency :: Event -> Double
e_latency = fromMaybe 0.1 . e_get_double "latency"

-- | Extract non-'reserved' 'Keys' from 'Event'.
e_parameters :: Event -> [(Key,Double)]
e_parameters =
    map (\(k,v) -> (k,f_double_err k v)) .
    filter k_is_parameter .
    Map.toList

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

-- * Event instrumentals

-- | Access 'I.Instr' at 'Event'.
e_instr :: Event -> Maybe I.Instr
e_instr = fmap f_instr . e_get "instr"

-- * Event temporals

-- | Merge two time-stamped 'Event' sequences.  Note that this uses
-- 'D.fwd' to calculate start times.
e_merge' :: (Time,[Event]) -> (Time,[Event]) -> [(Time,Event)]
e_merge' (pt,p) (qt,q) =
    let fwd = D.fwd . e_dur
        p_st = map (+ pt) (0 : scanl1 (+) (map fwd p))
        q_st = map (+ qt) (0 : scanl1 (+) (map fwd q))
    in t_merge (zip p_st p) (zip q_st q)

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
e_is_rest :: Event -> Bool
e_is_rest e =
    case e_get_double "rest" e of
      Just r -> r > 0
      Nothing -> False

-- * SC3

-- | Generate @SC3@ 'Bundle' messages describing 'Event'.  Consults the
-- 'instr_send_release' in relation to gate command.
e_bundle :: Time -> Int -> Event-> Maybe (Bundle,Bundle)
e_bundle t j e =
    let s = maybe "default" I.i_name (e_instr e)
        sr = maybe True I.i_send_release (e_instr e)
        p = e_pitch e
        d = e_dur e
        rt = D.occ d {- rt = release time -}
        f = P.freq p
        pr = ("freq",f)
             : ("midinote",P.midinote p)
             : ("delta",D.delta d)
             : ("sustain",rt)
             : ("amp",e_amp e)
             : e_parameters e
        i = fromMaybe j (e_id e)
        t' = t + realToFrac (e_latency e)
    in if e_is_rest e || isNaN f
       then Nothing
       else let m_on = e_type_match' e ([s_new s i AddToTail 1 pr]
                                       ,[n_set i pr]
                                       ,[])
                m_off = if not sr
                        then []
                        else e_type_match' e ([n_set i [("gate",0)]]
                                             ,[n_set i [("gate",0)]]
                                             ,[])
                m_on' = case I.i_synthdef =<< e_instr e of
                          Just sy -> d_recv sy : m_on
                          Nothing -> m_on
            in Just (Bundle t' m_on'
                    ,Bundle (t' + realToFrac rt) m_off)

-- | Transform (productively) a sorted 'Event' list into an 'NRT' score.
--
-- > e_nrt (replicate 5 e_empty)
-- > take 5 (nrt_bundles (e_nrt (repeat e_empty)))
e_nrt :: [Event] -> NRT
e_nrt =
    let cmb (o,c) r = span (<= o) (insert o (insert c r))
        rec r t i l =
            case l of
              [] -> r
              e:l' -> let t' = t + D.fwd (e_dur e)
                          i' = i + 1
                      in case e_bundle t i e of
                            Just p -> let (c,r') = cmb p r
                                      in c ++ rec r' t' i' l'
                            Nothing -> rec r t' i' l'
    in NRT . rec [] 0 1000

-- | Send 'Event' to @scsynth@ at 'Transport'.
e_send :: Transport m => Time -> Int -> Event -> m ()
e_send t j e =
    case e_bundle t j e of
      Just (p,q) -> sendBundle p >> sendBundle q
      Nothing -> return ()

-- | Function to audition a sequence of 'Event's using the @scsynth@
-- instance at 'Transport' starting at indicated 'Time'.
e_tplay :: Transport m => Time -> [Int] -> [Event] -> m ()
e_tplay t j e =
    case (j,e) of
      (_,[]) -> return ()
      ([],_) -> error ("e_tplay: no-id: " ++ show e)
      (i:j',d:e') -> do let t' = t + D.fwd (e_dur d)
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

-- * Temporal

-- | Left-biased merge of two sorted sequence of temporal values.
--
-- > let m = t_merge (zip [0,2,4,6] ['a'..]) (zip [0,3,6] ['A'..])
-- > in m == [(0,'a'),(0,'A'),(2,'b'),(3,'B'),(4,'c'),(6,'d'),(6,'C')]
t_merge :: Ord t => [(t,a)] -> [(t,a)] -> [(t,a)]
t_merge p q =
    case (p,q) of
      ([],_) -> q
      (_,[]) -> p
      ((t0,e0):r0,(t1,e1):r1) ->
            if t0 <= t1
            then (t0,e0) : t_merge r0 q
            else (t1,e1) : t_merge p r1

-- * Tuple

-- | Three tuple of /n/.
type T3 n = (n,n,n)

