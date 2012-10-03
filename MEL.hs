-- PART 2: A MEL interpreter

module MEL where

import World

{- OLEKS -1: Yes, this should've been in a different module. -}

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

{- OLEKS -1: The latest version of the assignment text asks you to let Program
be a type alias for Stm, but this is OKAY, just don't diverge like this in an
exam sittuation. -}

type Result = Maybe

initialWorld :: Maze -> World
initialWorld maze = World maze robot
             where robot = Robot (0,0) North [(0,0)]

newtype RobotCommand a = RC { runRC :: World -> Maybe (a, World) }
{- OLEKS -2: This type declaration does not indicate that a robot command
cannot modify the maze in the world as we asked for. -}

{- OLEKS 0: Also it would've been nice to know WHERE the robot failed, not just
that it faild. -}

instance Monad RobotCommand where
         return x = RC (\w -> Just (x, w))
         processor >>= processorGenerator = RC $ \w ->
                                   let Just (x, w') = runRC processor w
                                   in runRC (processorGenerator x) w'


{- OLEKS -2: This definition of bind EXPLICITLY allows for a RobotCommand to
modify the maze in the world. -}

        
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

{- OLEKS -2: Please use the more elegant do notation. Define functions

getRobot :: RobotCommand Robot

getMaze :: RobotCommand Maze

putRobot :: Robot -> RobotCommand ()

to get the robot/maze out of the monad and put the robot back in the monad
respectively. Then you should be able to make your interp somewhat more pretty
and reduce the boilerplate. -}

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


