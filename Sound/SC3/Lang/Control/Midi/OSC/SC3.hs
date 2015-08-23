-- | midi-osc-sc3
module Sound.SC3.Lang.Control.Midi.OSC.SC3 where

import Data.List.Split {- split -}

import Sound.OSC {- hsc3 -}
--import Sound.SC3 {- hsc3 -}

-- | (controller-id,node/group-id,name,min-value,max-value,warp,unit)
data Param = Param {param_cc_id :: Int
                   ,param_node_id :: Int
                   ,param_name :: String
                   ,param_min :: Double
                   ,param_max :: Double
                   ,param_warp :: String
                   ,param_unit :: String}
            deriving (Read,Show)

type Param' = (Int,Int,String,Double,Double,String,String)

param :: Param' -> Param
param (c,k,nm,l,r,w,u) = Param c k nm l r w u

parse_param_read :: String -> Param
parse_param_read = read

-- | csv = comma separated values
parse_param_csv :: String -> Param
parse_param_csv s =
    case splitOn "," s of
      [c,k,nm,l,r,w,u] -> param (read c,read k,nm,read l,read r,w,u)
      _ -> error "parse_param_csv"

-- | ws = white-space
parse_param_ws :: String -> Param
parse_param_ws s =
    case words s of
      [c,k,nm,l,r,w,u] -> param (read c,read k,nm,read l,read r,w,u)
      _ -> error "parse_param_ws"

param_to_datum :: Param -> [Datum]
param_to_datum cfg =
    let Param c k nm l r w u = cfg
    in [int64 c,int64 k,string nm,double l,double r,string w,string u]

param_to_message :: Param -> Message
param_to_message = message "/param" . param_to_datum

with_midi_osc_sc3 :: Connection UDP a -> IO a
with_midi_osc_sc3 = withTransport (openUDP "127.0.0.1" 57300)
