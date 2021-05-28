module Two where
import Data.Ratio (denominator, numerator, (%))

-- Converts a non-empty list of Integers to it's fractional representation
computeRat :: [Integer] -> Rational
computeRat (x : xs)
  | null xs = x % 1
  | otherwise = x % 1 + recip r
  where
    r = computeRat xs

-- Convert a fraction to its continued fraction representation
cf :: Rational -> [Integer]
cf x
  | denominator x == 1 = [n]
  | otherwise = whole : cf (recip fraction)
  where
    n = numerator x
    whole = n `quot` denominator x
    fraction = x - whole % 1