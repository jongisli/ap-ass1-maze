module World where

import qualified Data.Map as M
import Data.Maybe

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)

type Position = (Int, Int)

type Cell = [Direction]

data Maze = Mz (M.Map Position Cell) Int Int

go :: Int -> Direction -> Position -> Position
go n North (ew, ns) = (ew, ns+n)
go n South (ew, ns) = (ew, ns-n)
go n East (ew, ns) = (ew+n, ns)
go n West (ew, ns) = (ew-n, ns)

hasWall :: Maze -> Position -> Direction -> Bool
hasWall (Mz map _ _) pos dir = elem dir $ fromMaybe [] $ M.lookup pos map

validMove :: Maze -> Position -> Position -> Bool
validMove maze (x1, y1) (x2, y2) 
          | x1 == x2 && y1 == y2+1 = (not (hasWall maze (x1, y1) South))
                                     || (not (hasWall maze (x2, y2) North))
          | x1 == x2 && y1+1 == y2 = (not (hasWall maze (x1, y1) North))
                                     || (not (hasWall maze (x2, y2) South))
          | y1 == y2 && x1 == x2+1 = (not (hasWall maze (x1, y1) West))
                                     || (not (hasWall maze (x2, y2) East))
          | y1 == y2 && x1+1 == x2 = (not (hasWall maze (x1, y1) East))
                                     || (not (hasWall maze (x2, y2) West))