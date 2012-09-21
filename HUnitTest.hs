import Test.HUnit
import Data.Maybe (fromMaybe)
import World




test1Maze = fromList [((0,0),[North,South,West]),((0,1),[North,South,West]),((0,2),[South,West]),((0,3),[West,East]),((0,4),[North,West]),((1,0),[South]),((1,1),[North]),((1,2),[South,East]),((1,3),[North,West]),((1,4),[North,South,East]),((2,0),[North,South]),((2,1),[South,East]),((2,2),[West,East]),((2,3),[]),((2,4),[North,West,East]),((3,0),[North,South]),((3,1),[South,West]),((3,2),[West]),((3,3),[]),((3,4),[North,West,East]),((4,0),[North,South,East]),((4,1),[North,South,East]),((4,2),[North,South,East]),((4,3),[South,East]),((4,4),[North,West,East])]

test1 = TestCase $ assertBool "Go 3 north" $ (0,3) == (go 3 North (0,0))

test2 = TestCase $ assertBool "Check hasWall in testMase" $ (hasWall test1Maze (0,0) West) && (not $ hasWall test1Maze (0,0) East)

test3 = TestCase $ assertBool "Opposite direction" $ ((oppositeDirection (Robot (0,0) North [])) == South) && ((oppositeDirection (Robot (0,0) East [])) == West)

test4 = TestCase $ assertBool "Turn left or right" $ ((turnLeft (Robot (0,0) North [])) == West) && ((turnRight (Robot (0,0) North [])) == East)

test5 = TestCase $ assertBool "evalCond tests" $ (evalCond (And (Wall ToLeft) (Not AtGoalPos)) w)
         where w = (initialWorld test1Maze)

test6Program =(interp $ Block [While (Wall Ahead) TurnLeft, Forward, Forward, Backward, TurnRight])

test6 = TestCase $ assertBool "interp tests" $ (let m = (runRC test6Program (initialWorld test1Maze)) in case m of
      Nothing -> False 
      Just (_,w) -> ((position (robot w)) == (1,0)) && ((direction (robot w)) == South))

test7Maze = fromList [((0,0),[South,West]),((0,1),[North,West,East]),((1,0),[South,East]),((1,1),[North,East,West])]

test7Program = Program{statement=(Block [TurnRight,
   While (And (Not (AtGoalPos)) (Not $ Not $ Not $ AtGoalPos)) (If (Not (Wall Ahead)) Forward (Block [TurnLeft, Forward, TurnRight]))])} 

test7 = TestCase $ assertBool "runProg" $ (runProg test7Maze test7Program) == (Just ([(1,1),(1,0),(0,0)],East))

test8Program1 = Program{statement=(Block [TurnRight, Forward, Forward, Forward])}

test8Program2 = Program{statement=(Block [Forward, TurnRight, Forward])}

test8Maze = fromList [((0,0),[South,West]),((0,1),[North,West,East]),((1,0),[South]),((1,1),[North,East,West])]

test8 = TestCase $ assertBool "runProg, walking into a border and a wall" $ (runProg test8Maze test8Program1) == Nothing && (runProg test8Maze test8Program2) == Nothing

tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8]

main = runTestTT tests
