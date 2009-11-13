import qualified Data.Char as C
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import qualified Graphics.X11.Xlib.Misc as X
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Pattern.Step
import qualified System.Environment as E
import System.IO
import qualified System.Random as R
import qualified System.Timeout as T

data S = S { st :: UDP
           , sg :: R.StdGen
           , sd :: X
           , sk :: Char
           , sx :: Double
           , sy :: Double }
        deriving Show

data R = R { rt :: Double 
           , rp :: (Double, Double, Char) }
         deriving Show

instance R.RandomGen S where
    next g = let (i, g') = R.next (sg g)
             in (i, g { sg = g' })
    split g = let (g', g'') = R.split (sg g)
              in (g { sg = g' }, g { sg = g'' } )

-- | Real valued timeout
timeout :: Double -> IO a -> IO (Maybe a)
timeout t =
    let i = floor (t * 1000000)
    in T.timeout i

update :: (R, S) -> IO S
update (r, s) = do
  c <- timeout (rt r) getChar
  (x, y) <- x_ptr (sd s)
  let k' = maybe (sk s) id c
      s' = s { sx = x , sy = y, sk = k' }
      f = x * 200 + 600 + fromIntegral (C.ord k' * 9)
  send (st s') (n_set (-1) [("f", f), ("a", y)])
  print s'
  return s'

mk_s0 :: UDP -> R.StdGen -> X -> S
mk_s0 t g d = S t g d 'a' 0 0

x :: P S Double
x = prp (\s -> (pcons (sx s) x, s))

y :: P S Double
y = prp (\s -> (pcons (sy s) y, s))

k :: P S Char
k = prp (\s -> (pcons (sk s) k, s))

q :: P S R
q = pzipWith R (pwhite 0.05 1.25 pinf) (pzip3 x y k)

ping :: UGen
ping = let f = Control KR "f" 440
           a = Control KR "a" 0.1
       in out 0 (sinOsc AR f 0 * a)

main :: IO ()
main = do
  [a] <- E.getArgs
  hSetBuffering stdin NoBuffering
  g <- R.getStdGen
  d <- x_init a
  t <- openUDP "127.0.0.1" 57110
  reset t
  async t (d_recv (synthdef "ping" ping))
  send t (s_new "ping" (-1) AddToTail 1 [])
  r <- runP (mk_s0 t g d) update (flip (:)) [] q
  print (reverse r)

-- * X11 pointer access

-- | X11 connection state.
type X = (X.Display, X.Window, Double, Double)

-- | Initialize X11 connection.
x_init :: String -> IO X
x_init n = do
  d <- X.openDisplay n
  let r = X.defaultRootWindow d
  a <- X.getWindowAttributes d r
  let rw = 1.0 / fromIntegral (X.wa_width a)
      rh = 1.0 / fromIntegral (X.wa_height a)
  return (d, r, rw, rh)

-- | Read pointer location relative to root window.
x_ptr :: X -> IO (Double, Double)
x_ptr (d, r, rw, rh) = do
  (_, _, _, _, _, dx, dy, _) <- X.queryPointer d r 
  let x = fromIntegral dx * rw
      y = 1.0 - (fromIntegral dy * rh)
  return (x, y)
