-- PART 1: Modelling the world

module World(Maze,fromList,Direction(..),Position,Cell,hasWall,hasBorder,validMove, go, getWidth, getHeight) where

import qualified Data.Map as M
import Data.Maybe

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)

type Position = (Int, Int)

type Cell = [Direction]

data Maze = Maze { posToCell :: (M.Map Position Cell),
                   width :: Int,
                   height :: Int 
                   } deriving(Show)

getWidth :: Maze -> Int
getWidth m = width m

getHeight :: Maze -> Int
getHeight m = height m

go :: Int -> Direction -> Position -> Position
go n North (ew, ns) = (ew, ns+n)
go n South (ew, ns) = (ew, ns-n)
go n East (ew, ns) = (ew+n, ns)
go n West (ew, ns) = (ew-n, ns)

hasWall :: Maze -> Position -> Direction -> Bool
hasWall (Maze map _ _) pos dir = elem dir $ fromMaybe [] $ M.lookup pos map

hasBorder :: Maze -> Position -> Direction -> Bool
hasBorder (Maze _ w h) (0,_) West = True
hasBorder (Maze _ w h) (x,_) East = (x == w-1)
hasBorder (Maze _ w h) (_,0) South = True
hasBorder (Maze _ w h) (_,y) North = (y == h-1)
hasBorder _ _ _ = False

validMove :: Maze -> Position -> Direction -> Bool
validMove m p d = not ((hasWall m p d) || (hasBorder m p d))

fromList :: [(Position, [Direction])] -> Maze
fromList lst = Maze (M.fromList lst) (maximum xCoords + 1) (maximum yCoords + 1)
         where xCoords = [x | ((x,_), dir) <- lst]
               yCoords = [y | ((_,y), dir) <- lst]


