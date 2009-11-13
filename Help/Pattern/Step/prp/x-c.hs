import Control.Exception
import qualified Data.Char as C
import qualified Data.Map as M
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
           , sm :: M.Map String Double }
        deriving Show

type R = (Double, Double, Double)

instance R.RandomGen S where
    next g = let (i, g') = R.next (sg g)
             in (i, g { sg = g' })
    split g = let (g', g'') = R.split (sg g)
              in (g { sg = g' }, g { sg = g'' } )

getd :: S -> String -> Double
getd s k = M.findWithDefault 0 k (sm s)

-- | Real valued timeout
timeout :: Double -> IO a -> IO (Maybe a)
timeout t =
    let i = floor (t * 1000000)
    in T.timeout i

update :: (R, S) -> IO S
update ((rt,f,a), s) = do
  print s
  send (st s) (n_set (-1) [("f", f), ("a", a)])
  (x, y) <- x_ptr (sd s)
  c <- timeout rt getChar
  let k' = maybe (getd s "k") (fromIntegral . C.ord) c
  return (s { sm = M.fromList [("x", x)
                              ,("y", y)
                              ,("k", k')] })

mk_s0 :: UDP -> R.StdGen -> X -> S
mk_s0 t g d = S t g d (M.fromList [("x",0),("y",0),("k",0)])

ps :: String -> P S Double
ps x = prp (\s -> (pcons (getd s x) (ps x), s))

x, y, k :: P S Double
x = ps "x"
y = ps "y"
k = ps "k"

q :: P S R
q = pzip3 (pwhite 0.05 1.25 pinf) (x *. 300 +. 400 +. k *. 12) (y *. 0.25)

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
  finally
    (do reset t
        async t (d_recv (synthdef "ping" ping))
        send t (s_new "ping" (-1) AddToTail 1 [])
        r <- runP (mk_s0 t g d) update (flip (:)) [] q
        print (reverse r))
    (do reset t
        close t
        x_close d)

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

-- | Close X11 connection.
x_close :: X -> IO ()
x_close (d, _, _, _) = X.closeDisplay d

-- | Read pointer location relative to root window.
x_ptr :: X -> IO (Double, Double)
x_ptr (d, r, rw, rh) = do
  (_, _, _, _, _, dx, dy, _) <- X.queryPointer d r 
  let x = fromIntegral dx * rw
      y = 1.0 - (fromIntegral dy * rh)
  return (x, y)
