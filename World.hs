module World where

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)
type Position = (Int, Int)
type Cell = [Direction]

go :: Direction -> Position -> Position
go n North (ew, ns) = (ew, ns+n)
go n South (ew, ns) = (ew, ns-n)
go n East (ew, ns) = (ew+n, ns)
go n West (ew, ns) = (ew-n, ns)

go1 = go 1
--callNtimes :: Int -> (Position -> Position) -> Position
--callNtimes n f = foldr (.) id (replicate n f)

--goN :: Direction -> Position -> Int -> Position
--goN a pos n = (callNtimes n (go1 a)) pos 
