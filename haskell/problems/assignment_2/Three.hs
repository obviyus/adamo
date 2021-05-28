module Three where

import Data.Ratio (denominator, numerator, (%))
import Two ( computeRat )

phi :: Double
phi = (1 + sqrt 5) / 2

computeFrac :: Rational -> Double
computeFrac x = fromIntegral (numerator x) / fromIntegral (denominator x)

phiList :: [Integer]
phiList = [1, 1 ..]

approxGR :: Double -> Rational
approxGR = runner 1

-- Keep computing with more and more 1's until accuracy is < epsilon
runner :: Int -> Double -> Rational
runner x epsilon
  | abs (computeFrac approx - phi) < epsilon = approx
  | otherwise = runner (x + 1) epsilon
  where
    approx = computeRat (take x phiList)