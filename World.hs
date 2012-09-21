-- PART 1: Modelling the world

module World where

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


-- PART 2: A MEL interpreter

data Relative = Ahead | ToLeft | ToRight | Behind
              deriving (Eq, Show)

data Cond = Wall Relative
          | And  Cond Cond
          | Not  Cond
          | AtGoalPos
          deriving (Eq, Show)
            
data Stm = Forward
         | Backward
         | TurnRight
         | TurnLeft    
         | If Cond Stm Stm
         | While Cond Stm
         | Block [Stm]
         deriving (Eq, Show)

data Robot = Robot { position :: Position,
                     direction :: Direction,
                     history :: [Position] }
             deriving(Show)
data World = World { maze :: Maze,
                     robot :: Robot }
            deriving(Show)

data Program = Program {statement :: Stm}

type Result = Maybe

initialWorld :: Maze -> World
initialWorld maze = World maze robot
             where robot = Robot (0,0) North [(0,0)]

newtype RobotCommand a = RC { runRC :: World -> Maybe (a, World) }

instance Monad RobotCommand where
         return x = RC (\w -> Just (x, w))
         processor >>= processorGenerator = RC $ \w -> 
                                   let Just (x, w') = runRC processor w
                                   in runRC (processorGenerator x) w'
        
oppositeDirection :: Robot -> Direction
oppositeDirection robot = case (direction robot) of
                  North -> South
                  South -> North
                  East -> West
                  West -> East

turnRight :: Robot -> Direction
turnRight robot = case (direction robot) of
                  North -> East
                  South -> West
                  East -> South
                  West -> North

turnLeft :: Robot -> Direction
turnLeft robot = case (direction robot) of
                 North -> West
                 South -> East
                 East -> North
                 West -> South

evalCond :: Cond -> World -> Bool
evalCond (Wall relative) w = case relative of
                         Ahead -> hasWall mz pos dir
                         ToLeft -> hasWall mz pos (turnLeft (robot w))
                         ToRight -> hasWall mz pos (turnRight (robot w))
                         Behind -> hasWall mz pos (oppositeDirection (robot w))
                         where
                         mz = maze w
                         pos = position (robot w)
                         dir = direction (robot w)
evalCond (Not c) w = not (evalCond c w)
evalCond (And c1 c2) w = (evalCond c1 w) && (evalCond c2 w)
evalCond AtGoalPos w = (position robo) == (width',height')
                       where robo = robot w
                             width' = (width (maze w)) - 1
                             height' = (height (maze w)) - 1

interp :: Stm -> RobotCommand ()
interp Forward = RC (\w ->
    (let robo = robot w
         mze  = maze w in
    if (validMove mze (position robo) (direction robo)) then
       Just ((),
         World
	   mze
           (let p = (go 1 (direction robo) (position robo)) in
	   (Robot
	     p
	     (direction robo)
	     (p:(history robo)))))
    else
       Nothing
  ))
interp Backward = RC (\w ->
    (let robo = robot w
         mze  = maze w in
    if (validMove mze (position robo) (oppositeDirection robo)) then
       Just ((),
         World
	   mze
	   (let p = (go (-1) (direction robo) (position robo)) in
	   (Robot
	     p
	     (direction robo)
	     (p:(history robo)))))
    else
       Nothing
  ))

interp TurnRight = RC (\w -> do
    return ((),
      World
        (maze w)
        (Robot
          (position (robot w))
          (turnRight (robot w))
          (history (robot w)))))

interp TurnLeft = RC (\w -> do
    return ((),
      World
        (maze w)
        (Robot
          (position (robot w))
          (turnLeft (robot w))
          (history (robot w)))))

interp (If c s1 s2) = RC (\w -> 
    if (evalCond c w) then 
        (runRC (interp s1) w)
    else 
        (runRC (interp s2) w))

interp (While c s) = RC (\w ->
    if (not $ evalCond c w) then do
        return ((), w)
    else do
        (_,w') <- runRC (interp s) w
        (runRC (interp (While c s)) w'))

interp (Block []) = RC (\w -> Just((),w))
interp (Block (s:ss)) = RC (\w ->
                      do (_,w') <- runRC (interp s) w
                         (runRC (interp (Block ss)) w'))

runProg :: Maze -> Program -> Result ([Position], Direction)
runProg m p = let w = (initialWorld m)
                  rc = (interp (statement p)) in
                  do
                      (_,w') <-  (runRC rc) w
                      let robo = robot w'
                      return (history robo, direction robo)
