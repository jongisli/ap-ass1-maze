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

-- RobotCommand and it's Monad implementation heavily influenced
-- by the State monad.
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
    if not (hasWall (maze w) (position (robot w)) (direction (robot w))) then
       Just ((),
         World
	   (maze w)
           (let p = (go 1 (direction (robot w)) (position (robot w))) in
	   (Robot
	     p
	     (direction (robot w))
	     (p:(history (robot w))))))
    else
       Nothing
  )
interp Backward = RC (\w ->
    if not (hasWall (maze w) (position (robot w)) (oppositeDirection (robot w))) then
       Just ((),
         World
	   (maze w)
	   (let p = (go (-1) (direction (robot w)) (position (robot w))) in
	   (Robot
	     p
	     (direction (robot w))
	     (p:(history (robot w))))))
    else
       Nothing
  )

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
                 




-- Block case will be just like the [IO a] -> IO [a] from class

-- World :: RobotCommand World
-- setRobot :: Robot -> RobotCommand()

-- interp Forward = do w <- World
--                     let r = robot w
--                     r' <- do shit to the robot
--                     setRobot r'
