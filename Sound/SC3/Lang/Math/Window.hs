-- | Windowing functions.
module Sound.SC3.Lang.Math.Window where

import qualified Numeric.GSL.Special.Bessel as M {- hmatrix-special -}
import qualified Numeric.GSL.Special.Trig as M {- hmatrix-special -}

-- * Type and conversion

-- | A function from a \(0,1)\ normalised input to an output.
type Window x = x -> x

-- | A discrete /n/ element rendering of a 'Window'.
type Table x = [x]

-- | Generate an /n/ element table from a \(0,1)\ normalised window
-- function.
window_table :: (Integral n,Fractional a,Enum a) => n -> Window a -> Table a
window_table n f =
    let k = 1 / (fromIntegral n - 1)
    in map f [0,k..1]

-- * Math

-- | Regular modified Bessel function of fractional order zero.
bessel0 :: Double -> Double
bessel0 = M.bessel_Inu 0

-- | /n/ ^ 2.
square :: Num a => a -> a
square x = x * x

-- * Window functions

-- | Gaussian window, θ <= 0.5.
gaussian :: Floating a => a -> Window a
gaussian theta i = exp (- (0.5 * square ((i - 0.5) / (theta * 0.5))))

-- | Hann raised cosine window.
hann :: Floating a => Window a
hann i = 0.5 * (1 - cos (2 * pi * i))

-- | Hamming raised cosine window.
hamming :: Floating a => Window a
hamming i = 0.54 - 0.46 * cos (2 * pi * i)

-- | Kaiser windowing function, β is shape (1,2,8).
kaiser :: Double -> Window Double
kaiser beta i =
    let beta' = bessel0 beta
    in bessel0 (beta * sqrt (1 - ((2 * i - 1) ** 2))) / beta'

-- | 'M.sinc' window.
lanczos :: Window Double
lanczos i = M.sinc (2 * i - 1)

-- | Unit ('id') window, also known as a Dirichlet window.
rectangular :: Window a
rectangular = id

-- | 'sin' window.
sine :: Floating a => Window a
sine i = sin (i * pi)

-- | Triangular window, ie. Bartlett window with zero end-points.
triangular :: Fractional a => Window a
triangular i = 2 * (0.5 - abs (i - 0.5))

-- * Tables

-- | 'window_table' . 'gaussian'.
--
-- > import Sound.SC3.Plot
-- > plotTable [gaussian_table 1024 0.25,gaussian_table 1024 0.5]
gaussian_table :: (Integral n, Floating b, Enum b) => n -> b -> [b]
gaussian_table n = window_table n . gaussian

-- | 'window_table' . 'hamming'.
--
-- plotTable [hann_table 128,hamming_table 128]
hamming_table :: Int -> [Double]
hamming_table n = window_table n hamming

-- | 'window_table' . 'hann'.
--
-- plotTable [hann_table 128]
hann_table :: Int -> [Double]
hann_table n = window_table n hann

-- | 'window_table' . 'kaiser'.
--
-- let k = kaiser_table 128 in plotTable [k 1,k 2,k 8]
kaiser_table :: Int -> Double -> [Double]
kaiser_table n = window_table n . kaiser

-- | 'window_table' . 'lanczos'.
--
-- plotTable [lanczos_table (2^9)]
lanczos_table :: Integral n => n -> [Double]
lanczos_table n = window_table n lanczos

-- | 'window_table' . 'sine'.
--
-- plotTable [sine_table 128]
sine_table :: (Integral n, Floating b, Enum b) => n -> [b]
sine_table n = window_table n sine

-- | 'window_table' . 'triangular'.
--
-- plotTable [triangular_table (2^9)]
triangular_table :: (Integral n, Fractional b, Enum b) => n -> [b]
triangular_table n = window_table n triangular
