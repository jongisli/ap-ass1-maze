-- PART 2: A MEL interpreter

module MEL where

import World
import Data.Maybe

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
             deriving(Show, Eq)
data World = World { maze :: Maze,
                     robot :: Robot }
            deriving(Show)

data Program = Program {statement :: Stm}

type Result = Maybe

initialWorld :: Maze -> World
initialWorld maze = World maze robot
             where robot = Robot (0,0) North [(0,0)]

newtype RobotCommand a = RC { runRC :: World -> Maybe (a, Robot) }

instance Monad RobotCommand where
         return x = RC (\w -> Just (x, robot w))
         processor >>= processorGenerator = RC $ \w ->
             let a = runRC processor w in
             case a of
                 Just (x, r')-> runRC (processorGenerator x) (World (maze w) r')
                 Nothing -> Nothing

        
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
                             width' = (getWidth (maze w)) - 1
                             height' = (getHeight (maze w)) - 1

getRobot :: RobotCommand Robot
getRobot = RC (\w -> Just (robot w, robot w))

getMaze :: RobotCommand Maze
getMaze = RC (\w -> Just (maze w, robot w))

getWorld :: RobotCommand World
getWorld = RC (\w -> Just (w, robot w))

putRobot :: Robot -> RobotCommand ()
putRobot r = RC (\w -> Just ((), r))

putNothing :: RobotCommand ()
putNothing = RC (\w -> Nothing)

interp :: Stm -> RobotCommand ()

interp Forward = do 
       robo <- getRobot
       mze  <- getMaze
       let p = (go 1 (direction robo) (position robo))
       if (validMove mze (position robo) (direction robo)) then
           putRobot (Robot p (direction robo) (p:(history robo)))
       else
           putNothing

interp Backward = do 
       robo <- getRobot
       mze  <- getMaze
       let p = (go (-1) (direction robo) (position robo))
       if (validMove mze (position robo) (oppositeDirection robo)) then
           putRobot (Robot p (direction robo) (p:(history robo)))
       else
           putNothing


interp TurnRight = do
       robo <- getRobot
       putRobot (Robot (position robo) (turnRight robo) (history robo))


interp TurnLeft = do
       robo <- getRobot
       putRobot (Robot (position robo) (turnLeft robo) (history robo))

interp (If c s1 s2) = do
       w <- getWorld
       if (evalCond c w) then 
          interp s1
       else 
          interp s2

interp (While c s) = do
       w <- getWorld
       if (not $ evalCond c w) then do
           return ()
       else do
           interp s
           interp (While c s)

interp (Block []) = return ()
interp (Block (s:ss)) = do
       interp s
       interp (Block ss)

       
runProg :: Maze -> Program -> Result ([Position], Direction)
runProg m p = do
        let w = initialWorld m
        (_,r') <- runRC (interp (statement p)) w
        return (history r', direction r')