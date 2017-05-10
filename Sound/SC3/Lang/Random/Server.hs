module Sound.SC3.Lang.Random.Server where

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

import qualified Sound.SC3.Lang.Random.IO as L {- hsc3-lang -}

-- | Get current data from buffer, scramble it, and write it back.
buf_scramble :: (MonadIO m, DuplexOSC m) => Int -> m ()
buf_scramble n = do
  (_,sz,_,_) <- b_query1_unpack n
  b <- b_getn1_data n (0,sz - 1)
  r <- liftIO (L.scramble b)
  sendMessage (b_setn1 n 0 r)
