-- Logic: Collect all primes up to target number
--        Check if there exists a perfect log relationship
  
isPrimePower :: Integer -> Bool
isPrimePower x = any (\y -> y ^ (floor . logBase (fromIntegral y) . fromIntegral) x == x) (primesToN x)

primesToN :: Integer -> [Integer]
primesToN x = takeWhile (< x) (filter isPrime [2 ..])

isPrime :: Integer -> Bool
isPrime x = all (\y -> (x `rem` y) /= 0) [2 .. highest]
  where
    highest = floor (sqrt (fromIntegral x))

-- Relevant links:
-- https://stackoverflow.com/questions/39281632/check-if-a-number-is-a-perfect-power-of-another-number
-- https://stackoverflow.com/questions/1764163/explain-this-chunk-of-haskell-code-that-outputs-a-stream-of-primes
-- https://stackoverflow.com/questions/26416323/function-to-calculate-log-of-integer