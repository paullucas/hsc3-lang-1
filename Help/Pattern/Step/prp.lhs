> import Sound.SC3.Lang.Pattern.Step
> import System.IO

> data S = S { c :: Char }

> u :: Show a => (a, S) -> IO S
> u (a,_) = print a >> getChar >>= return . S

> s0 :: S
> s0 = S 'a'

> p :: P S Char
> p = prp (\s -> (pcons (c s) p, s))

> q :: P S (Char,Int)
> q = pzip p (pseq [1,2,3,4,5] 1)

> main :: IO ()
> main = do
>   hSetBuffering stdin NoBuffering
>   s <- u (('t',0), s0)
>   r <- runP s u (flip (:)) [] q
>   print (reverse r)

-- for below, set hsc3-literate-p to nil

import Sound.SC3.Lang.Pattern.Step
import System.IO

let { u (x,_) = print x >> getChar >>= return
    ; s0 = 'a'
    ; p = prp (\s -> (pcons s p, s))
    ; q = pzip p (pseq [1,2,3,4,5] 1) }
in do { hSetBuffering stdin NoBuffering
      ; s <- u (('t',0), s0)
      ; r <- runP s u (flip (:)) [] q
      ; print (reverse r) }
