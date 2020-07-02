{-# LANGUAGE OverloadedStrings #-}

module Rover.RoverSpec (spec) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Rover
import Test.Hspec
import Test.Hspec.Hedgehog ((===), Gen, assert, cover, diff, failure, forAll, hedgehog)
import Text.ParserCombinators.ReadP

problem :: String
problem =
  "5 5\n"
    ++ "1 2 N\n"
    ++ "LMLMLMLMM\n"

--- 1 2 N
-- L 1 2 W
-- M 0 2 W
-- L 0 2 S
-- M 0 1 S
-- L 0 1 E
-- M 1 1 E
-- L 1 1 N
-- M 2 1 N
-- M 3 1 N

parse :: ReadP a -> String -> Maybe a
parse p str =
  case x of
    [] -> Nothing
    [(x, _)] -> Just x
    _ -> Nothing
  where
    x = readP_to_S p str

genCoord :: Gen Coord
genCoord = do
  x <- Gen.integral (Range.linear (-100) 100)
  y <- Gen.integral (Range.linear (-100) 100)
  pure $ Coord x y

genInstruction :: Gen Instruction
genInstruction = Gen.choice [genMoveInstruction, genRotateInstruction]

genMoveInstruction :: Gen Instruction
genMoveInstruction = Gen.constant IMove

genRotateInstruction :: Gen Instruction
genRotateInstruction = Gen.element [IRight, ILeft]

genOrientation :: Gen Orientation
genOrientation = Gen.element $ enumFrom N

genRover :: Gen Rover
genRover = Rover <$> genCoord <*> genOrientation

spec :: Spec
spec = do
  describe "movement properties" $ do
    it "instructions will move the rover by at most 1 unit" $ hedgehog $ do
      rover <- forAll genRover
      inst <- forAll genInstruction
      let r = moveRover rover inst
      assert (roverDistance rover r <= 1)

    it "rotations do not change the rover coordinates" $ hedgehog $ do
      rover <- forAll genRover
      inst <- forAll genRotateInstruction
      let r = moveRover rover inst
      assert (roverDistance rover r == 0)

  describe "Mars" $ do
    it "calculates the correct distance" $ do
      distance (Coord 0 0) (Coord 0 0) `shouldBe` 0
      distance (Coord 0 0) (Coord 0 1) `shouldBe` 1
      distance (Coord 0 0) (Coord 1 0) `shouldBe` 1
      distance (Coord 0 0) (Coord 1 1) `shouldBe` 2
      distance (Coord 1 1) (Coord 1 1) `shouldBe` 0
      distance (Coord 1 1) (Coord 1 0) `shouldBe` 1
      distance (Coord 1 1) (Coord 1 1) `shouldBe` 0
      distance (Coord 1 1) (Coord 1 (-1)) `shouldBe` 2
      distance (Coord 1 1) (Coord (-1) (-1)) `shouldBe` 4
      distance (Coord 1 (-1)) (Coord (-1) (-1)) `shouldBe` 2
      distance (Coord (-1) (-1)) (Coord (-1) (-1)) `shouldBe` 0

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
      parse parseRover "1 2 N" `shouldBe` Just (Rover (Coord 1 2) N)
      parse parseRover "1 X N" `shouldBe` Nothing
    it "parses instruction lists" $ do
      parse (parseInstructions <* eof) "MMLR" `shouldBe` Just [IMove, IMove, ILeft, IRight]
    it "parses problem" $ do
      parse problemReader problem `shouldBe` Just [(Rover (Coord 1 2) N, [ILeft, IMove, ILeft, IMove, ILeft, IMove, ILeft, IMove, IMove])]
    it "moves the rover" $ do
      moveRover (Rover (Coord 0 0) N) IMove `shouldBe` Rover (Coord {x = 0, y = 1}) N
      moveRover (Rover (Coord 0 0) S) IMove `shouldBe` Rover (Coord {x = 0, y = -1}) S
      moveRover (Rover (Coord 0 0) E) IMove `shouldBe` Rover (Coord {x = 1, y = 0}) E
      moveRover (Rover (Coord 0 0) W) IMove `shouldBe` Rover (Coord {x = -1, y = 0}) W
      moveRover (Rover (Coord 0 0) N) IRight `shouldBe` Rover (Coord {x = 0, y = 0}) E
      moveRover (Rover (Coord 0 0) N) ILeft `shouldBe` Rover (Coord {x = 0, y = 0}) W

    it "runs the rover across mars" $ do
      runRover (Rover (Coord 0 0) N) [IMove] `shouldBe` Rover (Coord {x = 0, y = 1}) N
      runRover (Rover (Coord 0 0) N) [ILeft, IMove] `shouldBe` Rover (Coord {x = -1, y = 0}) W

    it "runs the many rovers across mars" $ do
      runRovers
        [ (Rover (Coord 0 0) N, [IMove]),
          (Rover (Coord 0 0) N, [ILeft, IMove])
        ]
        `shouldBe` [ Rover (Coord {x = 0, y = 1}) N,
                     Rover (Coord {x = -1, y = 0}) W
                   ]

    it "runs the examples" $ do
      runRovers <$> parseProblem problem
        `shouldBe` Just
          [ Rover (Coord 1 3) N
          ]

-- * x = Rover
-- 5 5
-- 1 2 N
-- LMLMLMLMM
--
-- 3 3 E
-- MMRMMRMRRM
--
