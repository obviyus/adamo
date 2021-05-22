positions :: [(Int, Int)]
positions = [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]

moves (x, y) all_moves = [(x + i, y + j) | i <- [-1, 1, -2, 2], j <- [-1, 1, -2, 2], abs (i - j) `elem` [1, 3], (x + i, y + j) `elem` positions, (x + i, y + j) `notElem` all_moves]

knightMove :: (Int, Int) -> Int -> [(Int, Int)]
knightMove (x, y) 0 = [(x, y)]
knightMove (x, y) n = runner (moves (x, y) []) [] n
  where
    runner all_moves _ 1 = all_moves
    runner [] all_moves n = runner all_moves [] (n - 1)
    runner (z : zs) all_moves n = runner zs (all_moves ++ moves z all_moves) n