module Sound.SC3.Lang.UI.X11 where

import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Data.Bits {- base -}
import System.IO.Unsafe {- base -}

import Graphics.X11.Xlib {- X11 -}
import Graphics.X11.Xlib.Extras {- X11 -}

-- * Types

type XQ = (Display,Window,Double,Double)
type ST = (Double,Double,Bool)
type UI = (XQ,ThreadId,MVar ST)

-- * XQ

xq_init :: IO XQ
xq_init = do
  _ <- initThreads
  d <- openDisplay ""
  let r = defaultRootWindow d
  a <- getWindowAttributes d r
  let r_width = 1.0 / fromIntegral (wa_width a)
      r_height = 1.0 / fromIntegral (wa_height a)
  return (d,r,r_width,r_height)

xq_read :: XQ -> IO ST
xq_read (d,r,r_width,r_height) = do
  p <- queryPointer d r
  let (_,_,_,_,_,dx,dy,rep_mask) = p
      mouseX = fromIntegral dx * r_width
      mouseY = 1.0 - (fromIntegral dy * r_height)
      mouseButton = rep_mask .&. button1Mask /= 0
  return (mouseX,mouseY,mouseButton)

xq_close :: XQ -> IO ()
xq_close (d,_,_,_) = closeDisplay d

-- * UI

msec_to_usec :: Num n => n -> n
msec_to_usec = (*) 1000

-- | Start UI thread with indicated query delay (in milli-seconds).
--
-- > ui <- ui_init 17
-- > mapM_ (\_ -> do {st <- ui_read ui;print st;threadDelay 500000}) [1..10]
-- > ui_end ui
ui_init :: Int -> IO UI
ui_init msec = do
  xq <- xq_init
  st <- xq_read xq
  v <- newMVar st
  th <- forkFinally
        (forever (do st' <- xq_read xq
                     _ <- swapMVar v st'
                     threadDelay (msec_to_usec msec)))
        (\_ -> xq_close xq)
  return (xq,th,v)

ui_read :: UI -> IO ST
ui_read (_,_,v) = readMVar v

ui_get_contents :: UI -> IO [ST]
ui_get_contents ui =
    unsafeInterleaveIO $ do
      x  <- ui_read ui
      xs <- ui_get_contents ui
      return (x : xs)

ui_end :: UI -> IO ()
ui_end (_,th,_) = killThread th

-- * Mouse

mouse_x :: UI -> IO Double
mouse_x = fmap (\(x,_,_) -> x) . ui_read

mouse_y :: UI -> IO Double
mouse_y = fmap (\(_,y,_) -> y) . ui_read

mouse_button :: UI -> IO Bool
mouse_button = fmap (\(_,_,b) -> b) . ui_read

-- > m <- mouse_st
mouse_st :: IO [ST]
mouse_st = do
  ui <- ui_init 17
  ui_get_contents ui
