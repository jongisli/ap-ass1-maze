-- PART 1: Modelling the world

module World where

import qualified Data.Map as M
import Data.Maybe

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)

type Position = (Int, Int)

type Cell = [Direction]

data Maze = Maze { posToCell :: (M.Map Position Cell),
                   height :: Int,
                   width :: Int 
                   } deriving(Show)

go :: Int -> Direction -> Position -> Position
go n North (ew, ns) = (ew, ns+n)
go n South (ew, ns) = (ew, ns-n)
go n East (ew, ns) = (ew+n, ns)
go n West (ew, ns) = (ew-n, ns)

hasWall :: Maze -> Position -> Direction -> Bool
hasWall (Maze map _ _) pos dir = elem dir $ fromMaybe [] $ M.lookup pos map

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

fromList :: [(Position, [Direction])] -> Maze
fromList lst = Maze (M.fromList lst) (maximum xCoords + 1) (maximum yCoords + 1)
         where xCoords = [x | ((x,_), dir) <- lst]
               yCoords = [y | ((_,y), dir) <- lst]

testMase = fromList [((0,0),[North,South,West]),((0,1),[North,South,West]),((0,2),[South,West]),((0,3),[West,East]),((0,4),[North,West]),((1,0),[South]),((1,1),[North]),((1,2),[South,East]),((1,3),[North,West]),((1,4),[North,South,East]),((2,0),[North,South]),((2,1),[South,East]),((2,2),[West,East]),((2,3),[]),((2,4),[North,West,East]),((3,0),[North,South]),((3,1),[South,West]),((3,2),[West]),((3,3),[]),((3,4),[North,West,East]),((4,0),[North,South,East]),((4,1),[North,South,East]),((4,2),[North,South,East]),((4,3),[South,East]),((4,4),[North,West,East])]

--- PART 2: A MEL interpreter

-- type Robot = Rbt