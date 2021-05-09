-- TODO: Do we really need to see all primes up to x?
isPrimePower :: Integer -> Bool
isPrimePower x = 
    any (\y -> y ^ (floor . logBase (fromIntegral y) . fromIntegral) x == x) (takeWhile (<=x) primes)

primes :: [Integer]
primes = sieve primes [2..] 

-- Optimized Seive from: https://stackoverflow.com/a/8871918/11940280
sieve :: Integral a => [a] -> [a] -> [a]
sieve ps (x:xs) = x : h ++ sieve pt [x | x <- t, rem x p /= 0]
     where (p:pt) = ps
           (h,t)  = span (< p*p) xs 