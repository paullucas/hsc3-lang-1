-- | An 'Event' is a ('Key','Field') map.
module Sound.SC3.Lang.Control.Event where

import Data.List {- base -}
import qualified Data.Map as Map {- containers -}
import Data.Maybe {- base -}
import Data.Monoid {- base -}
import Data.String {- base -}
import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import System.Random {- base -}

import qualified Sound.SC3.Lang.Collection as C
import qualified Sound.SC3.Lang.Control.Duration as D
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P
import qualified Sound.SC3.Lang.Math as M

-- * Field

-- | Event field.
--
-- 'Field's are 'Num'.
--
-- > 5 :: Field
-- > 4 + 5 :: Field
-- > negate 5 :: Field
-- > f_array [2,3] + f_array [4,5] == f_array [6,8]
-- > f_array [1,2,3] + f_array [4,5] == f_array [5,7,7]
-- > 4 + f_array [5,6] == f_array [9,10]
data Field = F_Double {f_double :: Double}
           | F_Vector {f_vector :: [Field]}
           | F_String {f_string :: String}
           | F_Instr {f_instr :: I.Instr}
             deriving (Eq,Show)

-- | Set of types that can be lifted to 'Field'.
class F_Value a where toF :: a -> Field
instance F_Value Bool where toF = F_Double . fromIntegral . fromEnum
instance F_Value Int where toF = F_Double . fromIntegral
instance F_Value Double where toF = F_Double
instance F_Value I.Instr where toF = F_Instr
instance F_Value Field where toF = id

-- | Numeric 'F_Value' types.
class F_Value a => F_Num a where
instance F_Num Int
instance F_Num Double
instance F_Num Field

-- | Maybe variant of 'f_double'.
f_double_m :: Field -> Maybe Double
f_double_m f = case f of {F_Double n -> Just n;_ -> Nothing;}

-- | Variant of /reader/ with specified error message.
f_reader_err :: String -> String -> (Field -> Maybe a) -> Field -> a
f_reader_err nm err f x =
    let s = nm ++ ": " ++ err ++ " (" ++ show x ++ ")"
    in fromMaybe (error s) (f x)

-- | Variant of 'f_double' with specified error message.
f_double_err :: String -> Field -> Double
f_double_err err = f_reader_err "f_double" err f_double_m

-- | Run '>' @0@ at 'f_double'.
f_bool_err :: String -> Field -> Bool
f_bool_err err = (> 0) . f_reader_err "f_bool" err f_double_m

-- | Run 'round' at 'f_double'.
f_int_err :: String -> Field -> Int
f_int_err err = round . f_reader_err "f_int" err f_double_m

-- | Single element 'F_Vector' constructor.
--
-- > f_ref 1 == f_array [1]
f_ref :: Field -> Field
f_ref = F_Vector . return

-- | Uniform vector constructor.
--
-- > f_array [1,2] == F_Vector [F_Double 1,F_Double 2]
f_array :: [Double] -> Field
f_array = F_Vector . map F_Double

-- | Maybe variant of 'f_vector'.
f_vector_m :: Field -> Maybe [Field]
f_vector_m f = case f of {F_Vector v -> Just v;_ -> Nothing;}

-- | 'length' of 'f_vector_m'.
--
-- > f_vector_length (f_array [1..5]) == Just 5
f_vector_length :: Field -> Maybe Int
f_vector_length = fmap length . f_vector_m

-- | Indexed variant of 'f_double_err'.
--
-- > f_double_err_ix "" Nothing 1 == 1
-- > f_double_err_ix "" (Just 1) (f_array [0,1]) == 1
f_double_err_ix :: String -> Maybe Int -> Field -> Double
f_double_err_ix err n =
    case n of
      Nothing -> f_double_err err
      Just i -> f_double_err err . (!! i) . f_vector

-- | Maybe variant of 'f_instr'.
f_instr_m :: Field -> Maybe I.Instr
f_instr_m f = case f of {F_Instr n -> Just n;_ -> Nothing;}

-- | Variant of 'f_instr' with specified error message.
f_instr_err :: String -> Field -> I.Instr
f_instr_err err = fromMaybe (error ("f_instr: " ++ err)) . f_instr_m

-- | Map /fn/ over vector elements at /f/.
--
-- > f_map negate (f_array [0,1]) == f_array [0,-1]
f_map :: (Field -> Field) -> Field -> Field
f_map fn f =
    case f of
      F_Vector l -> F_Vector (map fn l)
      _ -> error ("f_map: " ++ show f)

-- | Numerical unary operator.
--
-- > f_uop negate (F_Double 1) == F_Double (-1)
-- > f_uop negate (F_Vector [F_Double 0,F_Double 1]) == f_array [0,-1]
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
      (F_Vector v,F_Vector w) -> F_Vector (C.zipWith_c (f_binop f) v w)
      (F_Double _,F_Vector w) -> F_Vector (C.zipWith_c (f_binop f) [p] w)
      (F_Vector v,F_Double _) -> F_Vector (C.zipWith_c (f_binop f) v [q])
      _ -> error ("f_binop: " ++ show (p,q))

-- | At floating branch of 'Field'.
f_atf :: (Double -> a) -> Field -> a
f_atf f = f . f_double

-- | At floating branches of 'Field's.
f_atf2 :: (Double -> Double -> a) -> Field -> Field -> a
f_atf2 f p q =
    case (p,q) of
      (F_Double n1,F_Double n2) -> f n1 n2
      _ -> error ("f_atf2: " ++ show (p,q))

-- | At floating branches of 'Field's.
f_atf3 :: (Double -> Double -> Double -> a) -> Field -> Field -> Field -> a
f_atf3 f p q r =
    case (p,q,r) of
      (F_Double n1,F_Double n2,F_Double n3) -> f n1 n2 n3
      _ -> error ("f_atf3: " ++ show (p,q,r))

-- | Extend to 'Field' to /n/.
--
-- > f_mce_extend 3 (f_array [1,2]) == f_array [1,2,1]
-- > f_mce_extend 3 1 == f_array [1,1,1]
f_mce_extend :: Int -> Field -> Field
f_mce_extend n f =
    case f of
      F_Vector v -> F_Vector (take n (cycle v))
      _ -> F_Vector (replicate n f)

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

instance EqE Field
instance OrdE Field
instance RealFracE Field
instance UnaryOp Field
instance BinaryOp Field

-- * Key

-- | The type of the /key/ at an 'Event'.
--
-- > :set -XOverloadedStrings
-- > [K_dur,"pan"] == [K_dur,K_param "pan"]
data Key = K_degree | K_mtranspose | K_scale | K_stepsPerOctave
         | K_gtranspose | K_note | K_octave | K_root
         | K_ctranspose | K_harmonic | K_midinote
         | K_detune | K_freq
         | K_delta | K_dur | K_lag | K_legato | K_fwd' | K_stretch | K_sustain | K_tempo
         | K_db | K_amp
         | K_rest
         | K_instr | K_id | K_type | K_latency
         | K_param String
           deriving (Eq,Ord,Show)

instance IsString Key where
    fromString = K_param

-- | SC3 name of 'Key'.
--
-- > map k_name [K_freq,K_dur,K_param "pan"] == ["freq","dur","pan"]
k_name :: Key -> String
k_name k =
    case k of
      K_param nm -> nm
      _ -> drop 2 (show k)

-- | List of reserved 'Key's used in pitch, duration and amplitude
-- models.  These are keys that may be provided explicitly, but if not
-- will be calculated implicitly.
--
-- > (K_freq `elem` k_reserved) == True
k_reserved :: [Key]
k_reserved = [K_freq,K_midinote,K_note
             ,K_delta,K_sustain
             ,K_amp
             ,K_instr,K_id,K_type,K_latency,K_rest]

k_vector :: [Key]
k_vector = [K_scale]

-- | Is 'Key' /not/ 'k_reserved', and /not/ 'k_vector'.
--
-- > k_is_parameter (K_param "pan",0) == True
k_is_parameter :: (Key,a) -> Bool
k_is_parameter (k,_) = k `notElem` (k_reserved ++ k_vector)

-- * Event

-- | An 'Event' is a ('Key','Field') map.
type Event = Map.Map Key Field

-- | Insert (/k/,/v/) into /e/.
--
-- > e_get K_id (e_insert K_id 1 mempty) == Just 1
e_insert :: Key -> Field -> Event -> Event
e_insert k v = Map.insert k v

-- | Event from association list.
--
-- > e_get K_id (e_from_list [(K_id,1)]) == Just 1
e_from_list :: [(Key,Field)] -> Event
e_from_list = Map.fromList

-- | Event from association list.
--
-- > let a = [(K_id,1)] in e_to_list (e_from_list a) == a
e_to_list :: Event -> [(Key,Field)]
e_to_list = Map.toList

-- | Lookup /k/ in /e/.
--
-- > e_get K_id mempty == Nothing
e_get :: Key -> Event -> Maybe Field
e_get k = Map.lookup k

-- | Immediate or vector element lookup.
--
-- > e_get_ix Nothing K_id (e_from_list [(K_id,1)]) == Just 1
--
-- > let n = f_array [0,1,2]
-- > in e_get_ix Nothing K_id (e_from_list [(K_id,n)]) == Just n
--
-- > let n = f_array [0..9]
-- > in e_get_ix (Just 5) K_id (e_from_list [(K_id,n)]) == Just 5
e_get_ix :: Maybe Int -> Key -> Event -> Maybe Field
e_get_ix n k =
    case n of
      Nothing -> e_get k
      Just i -> fmap ((!! i) . f_vector) . e_get k

-- | Type specialised 'e_get'.
e_get_double :: Key -> Event -> Maybe Double
e_get_double k = fmap (f_double_err (k_name k)) . e_get k

-- | Type specialised 'e_get_ix'.
e_get_double_ix :: Maybe Int -> Key -> Event -> Maybe Double
e_get_double_ix n k = fmap (f_double_err (k_name k)) . e_get_ix n k

-- | Type specialised 'e_get'.
e_get_bool :: Key -> Event -> Maybe Bool
e_get_bool k = fmap (f_bool_err (k_name k)) . e_get k

-- | Type specialised 'e_get'.
e_get_int :: Key -> Event -> Maybe Int
e_get_int k = fmap (f_int_err (k_name k)) . e_get k

-- | Type specialised 'e_get_ix'.
e_get_int_ix :: Maybe Int -> Key -> Event -> Maybe Int
e_get_int_ix n k = fmap (f_int_err (k_name k)) . e_get_ix n k

-- | Type specialised 'e_get'.
e_get_instr :: Key -> Event -> Maybe I.Instr
e_get_instr k = fmap (f_instr_err (k_name k)) . e_get k

-- | Type specialised 'e_get_ix'.
e_get_instr_ix :: Maybe Int -> Key -> Event -> Maybe I.Instr
e_get_instr_ix n k = fmap (f_instr_err (k_name k)) . e_get_ix n k

-- | Type specialised 'e_get'.
e_get_array :: Key -> Event -> Maybe [Double]
e_get_array k = fmap (map (f_double_err (k_name k)) . f_vector) . e_get k

-- | Type specialised 'e_get_ix'.
--
-- > let e = e_from_list [(K_scale,f_array [0,2])]
-- > in e_get_array_ix Nothing K_scale e == Just [0,2]
--
-- > let e = e_from_list [(K_scale,f_ref (f_array [0,2]))]
-- > in e_get_array_ix (Just 0) K_scale e == Just [0,2]
e_get_array_ix :: Maybe Int -> Key -> Event -> Maybe [Double]
e_get_array_ix n k =
    fmap (map (f_double_err (k_name k)) . f_vector) .
    e_get_ix n k

-- | 'Event' /type/.
--
-- > e_type mempty == "s_new"
e_type :: Event -> String
e_type = fromMaybe "s_new" . fmap f_string . e_get K_type

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
-- > D.delta (e_dur Nothing mempty) == 1
-- > D.fwd (e_dur Nothing (e_from_list [(K_dur,1),(K_stretch,2)])) == 2
--
-- > let e = e_from_list [(K_dur,1),(K_legato,0.5)]
-- > in D.occ (e_dur Nothing e) == 0.5
e_dur :: Maybe Int -> Event -> D.Dur
e_dur n e =
    let f k = e_get_double_ix n k e
    in D.optDur (f K_tempo
                ,f K_dur
                ,f K_stretch
                ,f K_legato
                ,f K_sustain
                ,f K_delta
                ,f K_lag
                ,f K_fwd')

-- | Generate 'Pitch' from 'Event'.
--
-- > P.midinote (e_pitch Nothing mempty) == 60
-- > P.freq (e_pitch Nothing (e_from_list [(K_degree,5)])) == 440
--
-- > let e = e_from_list [(K_degree,5),(K_scale,f_array [0,2,3,5,7,8,10])]
-- > in P.midinote (e_pitch Nothing e) == 68
--
-- > let e = e_from_list [(K_degree,5),(K_scale,f_ref (f_array [0,2,3,5,7,8,10]))]
-- > in P.midinote (e_pitch (Just 0) (e_mce_expand e)) == 68
--
-- > P.freq (e_pitch Nothing (e_from_list [(K_midinote,69)])) == 440
e_pitch :: Maybe Int -> Event -> P.Pitch
e_pitch n e =
    let f k = e_get_double_ix n k e
    in P.optPitch (f K_mtranspose
                  ,f K_gtranspose
                  ,f K_ctranspose
                  ,f K_octave
                  ,f K_root
                  ,f K_degree
                  ,e_get_array_ix n K_scale e
                  ,f K_stepsPerOctave
                  ,f K_detune
                  ,f K_harmonic
                  ,f K_freq
                  ,f K_midinote
                  ,f K_note)

-- | 'Event' identifier.
e_id :: Maybe Int -> Event -> Maybe Int
e_id n = e_get_int_ix n K_id

-- | Lookup /db/ field of 'Event'.
--
-- > e_db Nothing mempty == (-20)
e_db :: Maybe Int -> Event -> Double
e_db n = fromMaybe (-20) . e_get_double_ix n K_db

-- | The linear amplitude of the amplitude model at /e/.
--
-- > e_amp Nothing (e_from_list [(K_db,-60)]) == 0.001
-- > e_amp Nothing (e_from_list [(K_amp,0.01)]) == 0.01
-- > e_amp Nothing mempty == 0.1
e_amp :: Maybe Int -> Event -> Double
e_amp n e = fromMaybe (M.dbamp (e_db n e)) (e_get_double_ix n K_amp e)

-- | Message /latency/ of event.
--
-- > e_latency mempty == 0.1
e_latency :: Event -> Double
e_latency = fromMaybe 0.1 . e_get_double K_latency

-- | Extract non-'reserved' 'Keys' from 'Event'.
--
-- > let e = e_from_list [(K_freq,1),(K_param "p",1),(K_scale,f_ref (f_array [0,3,7]))]
-- > in e_parameters Nothing e == [("p",1)]
e_parameters :: Maybe Int -> Event -> [(String,Double)]
e_parameters n =
    map (\(k,v) -> (k_name k,f_double_err_ix (k_name k) n v)) .
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

-- * Event temporal

-- | Merge two time-stamped 'Event' sequences.  Note that this uses
-- 'D.fwd' to calculate start times.
e_merge' :: (Time,[Event]) -> (Time,[Event]) -> [(Time,Event)]
e_merge' (pt,p) (qt,q) =
    let f = D.fwd . e_dur Nothing
        p_st = map (+ pt) (0 : scanl1 (+) (map f p))
        q_st = map (+ qt) (0 : scanl1 (+) (map f q))
    in t_merge (zip p_st p) (zip q_st q)

-- | Insert /fwd/ 'Key's into a time-stamped 'Event' sequence.
e_add_fwd :: [(Time,Event)] -> [Event]
e_add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' ->
          e_insert K_fwd' (F_Double (t1 - t0)) e0 : e_add_fwd ((t1,e1):e')
      _ -> map snd e

-- | Composition of 'add_fwd' and 'merge''.
e_merge :: (Time,[Event]) -> (Time,[Event]) -> [Event]
e_merge p q = e_add_fwd (e_merge' p q)

-- | N-ary variant of 'e_merge'.
--
-- > e_par [(0,repeat (e_from_list [(K_id,1)]))
-- >       ,(0,repeat (e_from_list [(K_param "b",2)]))
-- >       ,(0,repeat (e_from_list [(K_param "c",3)]))]
e_par :: [(Time,[Event])] -> [Event]
e_par l =
    case l of
      [] -> []
      [(_,p)] -> p
      (pt,p):(qt,q):r -> e_par ((min pt qt,e_merge (pt,p) (qt,q)) : r)

-- | 'mempty' with /rest/.
e_rest :: Event
e_rest = e_from_list [(K_rest,1)]

-- | Does 'Event' have a 'True' @rest@ key.
--
-- > e_is_rest mempty == False
-- > e_is_rest (e_from_list [(K_rest,1)]) == True
e_is_rest :: Event -> Bool
e_is_rest = fromMaybe False . e_get_bool K_rest

-- * MCE

-- | Maximum vector length at 'Event'.
--
-- > e_mce_depth (e_from_list [(K_id,1)]) == Nothing
-- > e_mce_depth (e_from_list [(K_id,1),(K_param "b",f_array [2,3])]) == Just 2
e_mce_depth :: Event -> Maybe Int
e_mce_depth e =
    let f = map snd (e_to_list e)
    in case mapMaybe f_vector_length f of
         [] -> Nothing
         l -> Just (maximum l)

-- | Extend vectors at 'Event' if required, returning 'e_mce_depth'.
--
-- > let {e = e_from_list [(K_id,f_array [1,2]),(K_param "b",f_array [2,3,4])]
-- >     ;r = e_from_list [(K_id,f_array [1,2,1]),(K_param "b",f_array [2,3,4])]}
-- > in e_mce_extend e == Just (3,r)
--
-- > let e = e_from_list [(K_id,1)]
-- > in e_mce_extend e == Nothing
e_mce_extend :: Event -> Maybe (Int,Event)
e_mce_extend e =
    let e' = e_to_list e
        flds = map snd e'
        f n = let flds' = map (f_mce_extend n) flds
              in (n,e_from_list (zip (map fst e') flds'))
    in fmap f (e_mce_depth e)

-- | 'e_mce_extend' variant.
e_mce_expand :: Event -> Event
e_mce_expand e = maybe e snd (e_mce_extend e)

-- | Parallel 'Event's, if required.
--
-- > let {e = e_from_list [(K_id,1),(K_param "b",f_array [2,3])]
-- >     ;r = [e_from_list [(K_id,1),(K_param "b",2)],e_from_list [(K_id,1),(K_param "b",3)]]}
-- > in e_un_mce e == Just r
--
-- > let {e = e_from_list [(K_id,f_array [1,2]),(K_param "b",f_array [3,4,5])]
-- >     ;r = e_from_list [(K_id,1),(K_param "b",5)]}
-- > in fmap (!! 2) (e_un_mce e) == Just r
--
-- > e_un_mce (e_from_list [(K_id,1)]) == Nothing
e_un_mce :: Event -> Maybe [Event]
e_un_mce e =
    let e' = e_to_list e
        flds = map snd e'
        f n = let flds' = transpose (map (f_vector . f_mce_extend n) flds)
              in map (e_from_list . zip (map fst e')) flds'
    in fmap f (e_mce_depth e)

-- | 'e_un_mce' variant.
e_un_mce' :: Event -> [Event]
e_un_mce' e = fromMaybe [e] (e_un_mce e)

-- * SC3

-- | Generate @SC3@ /(on,off)/ 'Message' sets describing 'Event'.
e_messages :: D.Dur -> Event -> Int -> Maybe Int -> Maybe (T2 [Message])
e_messages d e n_id n =
    let e_i = e_get_instr_ix n K_instr e
        s = maybe "default" I.i_name e_i
        sr = maybe True I.i_send_release e_i
        p = e_pitch n e
        rt = D.occ d {- rt = release time -}
        f = P.freq p
        pr = ("freq",f)
             : ("midinote",P.midinote p)
             : ("delta",D.delta d)
             : ("sustain",rt)
             : ("amp",e_amp n e)
             : e_parameters n e
        n_id' = fromMaybe n_id (e_id n e)
    in if e_is_rest e || isNaN f
       then Nothing
       else let m_on = e_type_match' e ([s_new s n_id' AddToTail 1 pr]
                                       ,[n_set n_id' pr]
                                       ,[])
                m_off = if not sr
                        then []
                        else e_type_match' e ([n_set n_id' [("gate",0)]]
                                             ,[n_set n_id' [("gate",0)]]
                                             ,[])
                m_on' = case I.i_synthdef =<< e_i of
                          Just sy -> d_recv sy : m_on
                          Nothing -> m_on
            in Just (m_on',m_off)

-- | MCE variant of 'e_messages'.
e_messages_mce :: D.Dur -> Event -> Int -> (Maybe (T2 [Message]),Int)
e_messages_mce d e n_id =
    let (r,n) = case e_mce_extend e of
                  Just (m,e') -> (zipWith (e_messages d e') [n_id ..] (map Just [0 .. m - 1]),m)
                  Nothing -> ([e_messages d e n_id Nothing],1)
    in case unzip (catMaybes r) of
         ([],[]) -> (Nothing,n_id)
         (m_on,m_off) -> (Just (concat m_on,concat m_off),n_id + n)

-- | Generate @SC3@ /(on,off)/ 'Bundle's describing 'Event'.
e_bundles :: Time -> Int -> D.Dur -> Event-> (Maybe (T2 Bundle),Int)
e_bundles t n_id d e =
    let rt = D.occ d {- rt = release time -}
        t' = t + realToFrac (e_latency e)
        t'' = t' + realToFrac rt
    in case e_messages_mce d e n_id of
         (Nothing,n_id') -> (Nothing,n_id')
         (Just (m_on,m_off),n_id') -> (Just (Bundle t' m_on,Bundle t'' m_off),n_id')

-- | Ordered sequence of 'Event'.
newtype Event_Seq = Event_Seq {e_seq_events :: [Event]}

-- | Transform 'Event_Seq' into a sequence of @SC3@ /(on,off)/ 'Bundles'.
--
-- > e_bundle_seq 0 (Event_Seq (replicate 5 mempty))
e_bundle_seq :: Time -> Event_Seq -> [T2 Bundle]
e_bundle_seq st =
    let rec t i l =
            case l of
              [] -> []
              e:l' -> let d = e_dur Nothing e
                          t' = t + D.fwd d
                          (b,i') = e_bundles t i d e
                      in b `mcons` rec t' i' l'
    in rec st 1000 . e_seq_events

-- | Transform (productively) an 'Event_Seq' into an 'NRT' score.
--
-- > let {n1 = nrt_bundles (e_nrt (Event_Seq (replicate 5 mempty)))
-- >     ;n2 = take 11 (nrt_bundles (e_nrt (Event_Seq (repeat mempty))))}
-- > in n1 == n2
e_nrt :: Event_Seq -> NRT
e_nrt =
    let rec r l =
            case l of
              [] -> r
              (o,c):l' -> let (c',r') = span (<= o) (insert o (insert c r))
                          in c' ++ rec r' l'
        g0 = Bundle 0 [g_new [(1,AddToTail,0)]]
    in NRT . (g0 :) . rec [] . e_bundle_seq 0

-- | Audition 'Event_Seq'.
e_play :: Transport m => Event_Seq -> m ()
e_play l = do
  let f t (p,q) = pauseThreadUntil (bundleTime p - 0.1) >>
                  run_bundle t p >>
                  sendBundle q
  st <- time
  mapM_ (f st) (e_bundle_seq st l)

instance Audible Event_Seq where play = e_play

-- * Aliases

-- | Type-specialised 'mempty'.
e_empty :: Event
e_empty = mempty

-- | Type-specialised 'mappend'.
--
-- > let {l = [(K_id,0)];r = [(K_degree,1)]}
-- > in e_from_list l <> e_from_list r == e_from_list (l <> r)
e_union :: Event -> Event -> Event
e_union = mappend

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

-- | Two tuple of /n/.
type T2 n = (n,n)

-- | Three tuple of /n/.
type T3 n = (n,n,n)

-- * List

-- | 'Maybe' variant of ':'.
mcons :: Maybe a -> [a] -> [a]
mcons e l = case e of {Just e' -> e' : l;Nothing -> l}
