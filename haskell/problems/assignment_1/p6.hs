type Player = String

type Points = Int

type Assits = Int

type Rebounds = Int

type Stats = (Points, Rebounds, Assits)

type GameStore = (Player, Points, Rebounds, Assits)

groupSame name [] = []
groupSame name ((n, p, r, a) : xs)
  | n == name = (n, p, r, a) : groupSame name xs
  | otherwise = groupSame name xs

addPoints :: Num p => [(a, p, c, d)] -> p
addPoints [] = 0
addPoints ((n, p, r, a) : xs) = p + addPoints xs

addRebounds :: Num p => [(a, b, p, d)] -> p
addRebounds [] = 0
addRebounds ((n, p, r, a) : xs) = r + addRebounds xs

addAssists :: Num p => [(a, b, c, p)] -> p
addAssists [] = 0
addAssists ((n, p, r, a) : xs) = a + addAssists xs

add :: (Num b, Num c1, Num c2) => [(a, b, c1, c2)] -> [(a, (b, c1, c2))]
add ((n, p, r, a) : xs) = [(n, (p + addPoints xs, r + addRebounds xs, a + addAssists xs))]

compileStats :: [GameStore] -> [(Player, Stats)]
compileStats ((n, p, r, a) : xs) = runner ((n, p, r, a) : xs) []
  where
    runner [] l = []
    runner ((n, p, r, a) : xs) l
      | n `notElem` l = add (groupSame n ((n, p, r, a) : xs)) ++ runner xs (n : l)
      | otherwise = runner xs l