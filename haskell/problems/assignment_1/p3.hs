-- Logic: Take tail of String and recursively add the multiplication of depth and tail

import Data.Char ( digitToInt )

octToInt :: String -> Int 
octToInt (x:xs)
    | null xs = digitToInt x
    | otherwise = digitToInt x * 8 ^ length xs + octToInt xs