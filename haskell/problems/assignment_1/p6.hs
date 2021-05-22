type Player = String
type Points = Int
type Assits = Int
type Rebounds = Int
type Stats = (Points, Rebounds, Assits)
type GameStore = (Player, Points, Rebounds, Assits)

compileStats :: [GameStore] -> [(Player, Stats)]
compileStats = foldl update []
  where
    update :: [(Player, Stats)] -> GameStore -> [(Player, Stats)]
    update gs (n, p, r, a)
      | Just (x, y, z) <- lookup n gs = (n, (p + x, r + y, a + z)) : filter (\(x, _) -> x /= n) gs
      | Nothing        <- lookup n gs = (n, (p, r, a)) : gs