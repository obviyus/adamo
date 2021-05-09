type Player = String

type Points = Int

type Assits = Int

type Rebounds = Int

type Stats = (Points, Rebounds, Assits)

type GameStore = (Player, Points, Rebounds, Assits)

compileStats :: [GameStore] -> [(Player, Stats)]
compileStats (x:xs)
    | null xs = 