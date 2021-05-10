-- Recursively reduce the array space while building up the intermediate steps
josephus :: Int -> [(Int, Int)]
josephus n = runner [1 .. n]
  where
    runner [y] = []
    runner (x : y : z) = (x, y) : runner (z ++ [x])

-- Read the last value of Josephus Winner
josephusWinner :: Int -> Int 
josephusWinner = fst . last . josephus

-- Direct formula application for final safe position
-- [https://en.wikipedia.org/wiki/Josephus_problem#Solution]
josephusWinnerFast :: Int -> Int
josephusWinnerFast n = 2 * (n - 2 ^ (floor . logBase 2 . fromIntegral) n) + 1