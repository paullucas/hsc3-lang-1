-- | An 'Event' is a ('Key',/value/) map with associated meta data.
module Sound.SC3.Lang.Control.Event where

import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import qualified Sound.OSC as O {- hosc -}
import qualified Sound.SC3 as S {- hsc3 -}

import qualified Sound.SC3.Lang.Control.Duration as D
import qualified Sound.SC3.Lang.Control.Instrument as I
import qualified Sound.SC3.Lang.Control.Pitch as P

-- | The type of the /key/ at an 'Event'.
type Key = String

{-
-- | The type of the /value/ at an 'Event'.
type Value = Double
-}

-- | The /type/ of an 'Event'.
data Type = E_s_new | E_n_set | E_rest deriving (Eq,Show)

-- | An 'Event' has a 'Type', possibly an integer identifier, possibly
-- an 'I.Instrument' and a 'M.Map' of ('Key','Value') pairs.
data Event a = Event {e_type :: Type
                     ,e_id :: Maybe Int
                     ,e_instrument :: Maybe I.Instrument
                     ,e_map :: M.Map Key a}
               deriving (Eq,Show)

-- | The /default/ empty event.
defaultEvent :: Event a
defaultEvent =
    Event {e_type = E_s_new
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.empty}

-- | Lookup /k/ in /e/.
--
-- > lookup_m "k" defaultEvent == Nothing
lookup_m :: Key -> Event a -> Maybe a
lookup_m k = M.lookup k . e_map

-- | Variant of 'lookup_m' with a default value /v/.
--
-- > lookup_v 1 "k" defaultEvent == 1
lookup_v :: a -> Key -> Event a -> a
lookup_v v k = fromMaybe v . lookup_m k

-- | Variant of 'lookup_v' with a transformation function.
--
-- > lookup_t 1 negate "k" defaultEvent == 1
-- > lookup_t 1 negate "k" (insert "k" 1 defaultEvent) == -1
lookup_t :: t -> (a -> t) -> Key -> Event a -> t
lookup_t v f k = maybe v f . lookup_m k

-- | Lookup /pitch/ model parameters at /e/ and construct a 'P.Pitch'
-- value.
--
-- > P.midinote (pitch defaultEvent) == 60
pitch :: (RealFrac a,Floating a) => Event a -> P.Pitch a
pitch = P.alist_to_pitch . M.toList . e_map

-- | Lookup /duration/ model parameters at /e/ and construct a
-- 'D.Duration' value.
--
-- > D.delta (duration defaultEvent) == 1
duration :: (Num a,Fractional a) => Event a -> D.Duration a
duration = D.alist_to_duration . M.toList . e_map

-- | Insert (/k/,/v/) into /e/.
--
-- > lookup_m "k" (insert "k" 1 defaultEvent) == Just 1
insert :: Key -> a -> Event a -> Event a
insert k v e = e {e_map = M.insert k v (e_map e)}

-- | Lookup /db/ field of 'Event', the default value is @-20db@.
db :: Num a => Event a -> a
db = lookup_v (-20) "db"

-- | Function to convert from decibels to linear amplitude.
dbAmp' :: Floating a => a -> a
dbAmp' a = 10 ** (a * 0.05)

-- | The linear amplitude of the amplitude model at /e/.
--
-- > amp (event [("db",-20)]) == 0.1
amp :: Floating a => Event a -> a
amp e = lookup_v (dbAmp' (db e)) "amp" e

-- | The /fwd/ value of the duration model at /e/.
--
-- > fwd (event [("dur",1),("stretch",2)]) == 2
fwd :: (Num a,Fractional a) => Event a -> a
fwd = D.fwd . duration

-- | The /latency/ to compensate for when sending messages based on
-- the event.  Defaults to @0.1@.
latency :: Fractional a => Event a -> a
latency = lookup_v 0.1 "latency"

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
           ,"amp"]

-- | Is 'Key' 'reserved'.
is_parameter :: (Key,a) -> Bool
is_parameter (k,_) = k `notElem` reserved

-- | Extract non-'reserved' 'Keys' from 'Event'.
parameters :: Event a -> [(Key,a)]
parameters = filter is_parameter . M.toList . e_map

-- | 'Value' editor for 'Key' at 'Event', with default value in case
-- 'Key' is not present.
edit_v :: Key -> a -> (a -> a) -> Event a -> Event a
edit_v k v f e =
    case lookup_m k e of
      Just n -> insert k (f n) e
      Nothing -> insert k (f v) e

-- | Variant of 'edit_v' with no default value.
edit :: Key -> (a -> a) -> Event a -> Event a
edit k f e =
    case lookup_m k e of
      Just n -> insert k (f n) e
      Nothing -> e

-- | Basic 'Event' constructor function with 'e_map' given as a list.
from_list :: Type -> Maybe Int -> Maybe I.Instrument -> [(Key,a)] -> Event a
from_list t n i l =
    Event {e_type = t
          ,e_id = n
          ,e_instrument = i
          ,e_map = M.fromList l}

-- | Construct an 'Event' from a list of (/key/,/value/) pairs.
--
-- > lookup_m "k" (event [("k",1)]) == Just 1
event :: [(Key,a)] -> Event a
event l =
    Event {e_type = E_s_new
          ,e_id = Nothing
          ,e_instrument = Nothing
          ,e_map = M.fromList l}

-- | Extract 'I.Instrument' name from 'Event', or @default@.
instrument_name :: Event a -> String
instrument_name e =
    case e_instrument e of
      Nothing -> "default"
      Just (I.InstrumentDef s _) -> S.synthdefName s
      Just (I.InstrumentName s _) -> s

-- | Extract 'I.Instrument' definition from 'Event' if present.
instrument_def :: Event a -> Maybe S.Synthdef
instrument_def e =
    case e_instrument e of
      Nothing -> Nothing
      Just (I.InstrumentDef s _) -> Just s
      Just (I.InstrumentName _ _) -> Nothing

-- | 'I.send_release' of 'I.Instrument' at 'Event'.
instrument_send_release :: Event a -> Bool
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

-- | Times are /hosc/ (NTP) times.
type Time = O.Time

-- | Merge two time-stamped 'Event' sequences.  Note that this uses
-- 'fwd' to calculate start times.
merge' :: (Ord t,Num t,Fractional t,Num a,Fractional a,Real a) =>
          (t,[Event a]) -> (t,[Event a]) -> [(t,Event a)]
merge' (pt,p) (qt,q) =
    let p_st = map (+ pt) (0 : scanl1 (+) (map (realToFrac.fwd) p))
        q_st = map (+ qt) (0 : scanl1 (+) (map (realToFrac.fwd) q))
    in f_merge (zip p_st p) (zip q_st q)

-- | Insert /fwd/ 'Key's into a time-stamped 'Event' sequence.
add_fwd :: (Num t,Real t,Fractional a) => [(t,Event a)] -> [Event a]
add_fwd e =
    case e of
      (t0,e0):(t1,e1):e' ->
          insert "fwd'" (realToFrac (t1 - t0)) e0 : add_fwd ((t1,e1):e')
      _ -> map snd e

-- | Composition of 'add_fwd' and 'merge''.
merge :: (Fractional a,Real a) => (Time,[Event a]) -> (Time,[Event a]) -> [Event a]
merge p q = add_fwd (merge' p q)

-- | Does 'Event' have a non-zero @rest@ key.
is_rest :: (Ord a,Num a) => Event a -> Bool
is_rest e =
    case lookup_m "rest" e of
      Just r -> r > 0
      Nothing -> False

-- * SC3

class (Ord a,Num a,Fractional a,Real a,RealFloat a) => Value a where
instance Value Double

-- | Generate @SC3@ 'O.Bundle' messages describing 'Event'.  Consults the
-- 'instrument_send_release' in relation to gate command.
to_sc3_bundle :: Value a => Time -> Int -> Event a -> Maybe (O.Bundle,O.Bundle)
to_sc3_bundle t j e =
    let s = instrument_name e
        sr = instrument_send_release e
        p = pitch e
        d = duration e
        rt = D.sustain d {- rt = release time -}
        f = P.detunedFreq p
        pr = ("freq",f)
             : ("midinote",P.midinote p)
             : ("note",P.note p)
             : ("delta",D.delta d)
             : ("sustain",rt)
             : ("amp",amp e)
             : parameters e
        pr' = map (\(k,v) -> (k,realToFrac v)) pr
        i = fromMaybe j (e_id e)
        t' = t + realToFrac (latency e)
    in if is_rest e || isNaN f
       then Nothing
       else let m_on = case e_type e of
                         E_s_new -> [S.s_new s i S.AddToTail 1 pr']
                         E_n_set -> [S.n_set i pr']
                         E_rest -> []
                m_off = if not sr
                        then []
                        else case e_type e of
                               E_s_new -> [S.n_set i [("gate",0)]]
                               E_n_set -> [S.n_set i [("gate",0)]]
                               E_rest -> []
            in Just (O.Bundle t' m_on
                    ,O.Bundle (t' + realToFrac rt) m_off)

-- | Send 'Event' to @scsynth@ at 'Transport'.
e_send :: (Value a,O.Transport m) => Time -> Int -> Event a -> m ()
e_send t j e =
    let voidM a = a >> return ()
    in case to_sc3_bundle t j e of
        Just (p,q) -> do case instrument_def e of
                           Just d -> voidM (S.async (S.d_recv d))
                           Nothing -> return ()
                         O.sendBundle p
                         O.sendBundle q
        Nothing -> return ()

-- | Function to audition a sequence of 'Event's using the @scsynth@
-- instance at 'Transport' starting at indicated 'Time'.
e_tplay :: (Value a,O.Transport m) => Time -> [Int] -> [Event a] -> m ()
e_tplay t j e =
    case (j,e) of
      (_,[]) -> return ()
      ([],_) -> error "e_tplay: no-id"
      (i:j',d:e') -> do let t' = t + realToFrac (fwd d)
                        e_send t i d
                        O.pauseThreadUntil t'
                        e_tplay t' j' e'

-- | Variant of 'e_tplay' with current clock time from 'time' as start
-- time.  This function is used to implement the pattern instances of
-- 'Audible'.
e_play :: (Value a,O.Transport m) => [Int] -> [Event a] -> m ()
e_play lj le = do
  st <- O.time
  e_tplay st lj le

{-
-- | The frequency of the 'pitch' of /e/.
--
-- > freq (event [("degree",5)]) == 440
-- > freq (event [("midinote",69)]) == 440
freq :: Event -> Double
freq = P.detunedFreq . pitch

-- | The /sustain/ value of the duration model at /e/.
--
-- > sustain (event [("dur",1),("legato",0.5)]) == 0.5
sustain :: Event -> Double
sustain = D.sustain . duration
-}
