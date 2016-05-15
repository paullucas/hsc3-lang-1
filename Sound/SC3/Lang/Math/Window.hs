-- | Windowing functions not at "Sound.SC3.Common.Math.Window" due
-- to "hmatrix-special" dependency.
module Sound.SC3.Lang.Math.Window where

import Sound.SC3.Common.Math.Window {- hsc3 -}

import qualified Numeric.GSL.Special.Bessel as M {- hmatrix-special -}
import qualified Numeric.GSL.Special.Trig as M {- hmatrix-special -}

-- | Regular modified Bessel function of fractional order zero.
bessel0 :: Double -> Double
bessel0 = M.bessel_Inu 0

-- | Kaiser windowing function, β is shape (1,2,8).
kaiser :: Double -> Window Double
kaiser beta i =
    let beta' = bessel0 beta
    in bessel0 (beta * sqrt (1 - ((2 * i - 1) ** 2))) / beta'

-- | 'M.sinc' window.
lanczos :: Window Double
lanczos i = M.sinc (2 * i - 1)

-- | 'window_table' . 'kaiser'.
--
-- > let k = kaiser_table 128 in plotTable [k 1,k 2,k 8]
kaiser_table :: Int -> Double -> [Double]
kaiser_table n = window_table n . kaiser

-- | 'window_table' . 'lanczos'.
--
-- plotTable [lanczos_table (2^9)]
lanczos_table :: Integral n => n -> [Double]
lanczos_table n = window_table n lanczos
