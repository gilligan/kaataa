module Rover.RoverSpec (spec) where

import Test.Hspec
import Rover
import Text.ParserCombinators.ReadP

problem :: String
problem =
    "5 5\n"
  ++ "1 2 N\n"
  ++ "LMLMLMLMM\n"


parse :: ReadP a -> String -> Maybe a
parse p str =
  case x of
    [] -> Nothing
    [(x, _)] -> Just x
    _ -> Nothing
  where
    x = readP_to_S p str

spec :: Spec
spec = do
  describe "Mars" $ do
      it "parses orientation correctly" $ do
        parse orientationReader "E" `shouldBe` Just E
        parse orientationReader "N" `shouldBe` Just N
        parse orientationReader "S" `shouldBe` Just S
        parse orientationReader "W" `shouldBe` Just W
        parse orientationReader "" `shouldBe` Nothing
        parse orientationReader "X" `shouldBe` Nothing
      it "parses coordinate correctly" $ do
        parse coordinateReader "0 0" `shouldBe` Just (Coord 0 0)
        parse coordinateReader "10 10" `shouldBe` Just (Coord 10 10)
        parse coordinateReader "a b" `shouldBe` Nothing
      it "parses an instruction correctly" $ do
        parse instructionReader "M" `shouldBe` Just IMove
        parse instructionReader "L" `shouldBe` Just ILeft
        parse instructionReader "R" `shouldBe` Just IRight
      it "parses a Rover correctly" $ do
        parse parseRover "1 2 N" `shouldBe` (Just (Rover (Coord 1 2) N))
        parse parseRover "1 X N" `shouldBe` Nothing
      it "parses instruction lists" $ do
        parse (parseInstructions <* eof) "MMLR" `shouldBe` Just [IMove, IMove, ILeft, IRight]
      it "parses problem" $ do
        parse parseProblem problem `shouldBe` Just [((Rover (Coord 1 2) N), [ILeft, IMove, ILeft, IMove, ILeft, IMove, ILeft, IMove, IMove ])]
      it "moves the rover" $ do
        moveRover (Rover (Coord 0 0) N) IMove `shouldBe` Rover (Coord { x = 0, y = 1}) N
        moveRover (Rover (Coord 0 0) S) IMove `shouldBe` Rover (Coord { x = 0, y = -1}) S
        moveRover (Rover (Coord 0 0) E) IMove `shouldBe` Rover (Coord { x = 1, y = 0}) E
        moveRover (Rover (Coord 0 0) W) IMove `shouldBe` Rover (Coord { x = -1, y = 0}) W
        moveRover (Rover (Coord 0 0) N) IRight `shouldBe` Rover (Coord { x = 0, y = 0}) E
        moveRover (Rover (Coord 0 0) N) ILeft `shouldBe` Rover (Coord { x = 0, y = 0}) W

      it "runs the rover across mars" $ do
        runRover (Rover (Coord 0 0) N) [IMove] `shouldBe` Rover (Coord { x = 0, y = 1}) N
        runRover (Rover (Coord 0 0) N) [ILeft, IMove] `shouldBe` Rover (Coord { x = -1, y = 0}) W
-- 5 5
-- 1 2 N
-- LMLMLMLMM
--
-- 3 3 E
-- MMRMMRMRRM
--
