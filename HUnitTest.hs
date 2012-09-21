import Test.HUnit
import Data.Maybe (fromMaybe)
import World

test1 = TestCase $ assertBool "Go 3 north" $ (0,3) == (go 3 North (0,0))

test2 = TestCase $ assertBool "Check hasWall in testMase" $ (hasWall testMase (0,0) West) && (not $ hasWall testMase (0,0) East)

--test3 = TestCase $ assertBool "" $ (World testMase (Robot (0,0) North [(0,0)])) == initialWorld testMase

test3 = TestCase $ assertBool "Opposite direction" $ ((oppositeDirection (Robot (0,0) North [])) == South) && ((oppositeDirection (Robot (0,0) East [])) == West)

test4 = TestCase $ assertBool "Turn left or right" $ ((turnLeft (Robot (0,0) North [])) == West) && ((turnRight (Robot (0,0) North [])) == East)

test5 = TestCase $ assertBool "evalCond tests" $ (evalCond (And (Wall ToLeft) (Not AtGoalPos)) w)
         where w = (initialWorld testMase)

test6 = TestCase $ assertBool "interp tests" $ (let (_,w) = (runRC (interp Block [While (Wall Ahead) TurnLeft, Forward, Forward, Backward, TurnRight]) (initialWorld testMase)) in
   ((position (robot w)) == (1,0)) && ((direction (robot w)) == South))

tests = TestList [test1, test2, test3, test4, test5]

main = runTestTT tests
