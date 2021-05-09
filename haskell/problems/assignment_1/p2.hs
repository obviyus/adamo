-- Logic: Keep dividing the target number by 8 until we reach 0, then return the remainder

intToOct :: Int -> String
intToOct x
    | quot x 8 == 0 = show (rem x 8)
    | otherwise = intToOct (quot x 8) ++ show (rem x 8)