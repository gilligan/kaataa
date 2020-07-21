{-# LANGUAGE OverloadedStrings #-}

module Rover.RoverSpec (spec) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Rover
import RoverParser
import RoverTypes
import Test.Hspec
import Test.Hspec.Hedgehog (Gen, assert, forAll, hedgehog)
import Text.ParserCombinators.ReadP

problem :: String
problem =
  "5 5\n"
    ++ "1 2 N\n"
    ++ "LMLMLMLMM\n"

roverDistance :: Rover -> Rover -> Int
roverDistance (MarsRover c1 _) (MarsRover c2 _) = distance c1 c2

distance :: Position Int -> Position Int -> Int
distance (Position x1 y1) (Position x2 y2) = abs (x1 - x2) + abs (y1 - y2)

parse :: ReadP a -> String -> Maybe a
parse p str =
  case x of
    [] -> Nothing
    [(x, _)] -> Just x
    _ -> Nothing
  where
    x = readP_to_S p str

genCoord :: Gen (Position Int)
genCoord = do
  x <- Gen.integral (Range.linear (-100) 100)
  y <- Gen.integral (Range.linear (-100) 100)
  pure $ Position x y

genInstruction :: Gen Instruction
genInstruction = Gen.choice [genMoveInstruction, genRotateInstruction]

genMoveInstruction :: Gen Instruction
genMoveInstruction = Gen.constant IMove

genRotateInstruction :: Gen Instruction
genRotateInstruction = Gen.element [IRight, ILeft]

genOrientation :: Gen Orientation
genOrientation = Gen.element $ enumFrom N

genRover :: Gen Rover
genRover = MarsRover <$> genCoord <*> genOrientation

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
      distance (Position 0 0) (Position 0 0) `shouldBe` 0
      distance (Position 0 0) (Position 0 1) `shouldBe` 1
      distance (Position 0 0) (Position 1 0) `shouldBe` 1
      distance (Position 0 0) (Position 1 1) `shouldBe` 2
      distance (Position 1 1) (Position 1 1) `shouldBe` 0
      distance (Position 1 1) (Position 1 0) `shouldBe` 1
      distance (Position 1 1) (Position 1 1) `shouldBe` 0
      distance (Position 1 1) (Position 1 (-1)) `shouldBe` 2
      distance (Position 1 1) (Position (-1) (-1)) `shouldBe` 4
      distance (Position 1 (-1)) (Position (-1) (-1)) `shouldBe` 2
      distance (Position (-1) (-1)) (Position (-1) (-1)) `shouldBe` 0

    it "parses orientation correctly" $ do
      parse orientation "E" `shouldBe` Just E
      parse orientation "N" `shouldBe` Just N
      parse orientation "S" `shouldBe` Just S
      parse orientation "W" `shouldBe` Just W
      parse orientation "" `shouldBe` Nothing
      parse orientation "X" `shouldBe` Nothing

    it "parses coordinate correctly" $ do
      parse coordinates "0 0" `shouldBe` Just (Position 0 0)
      parse coordinates "10 10" `shouldBe` Just (Position 10 10)
      parse coordinates "a b" `shouldBe` Nothing

    it "parses an instruction correctly" $ do
      parse instruction "M" `shouldBe` Just IMove
      parse instruction "L" `shouldBe` Just ILeft
      parse instruction "R" `shouldBe` Just IRight

    it "parses a Rover correctly" $ do
      parse rover "1 2 N" `shouldBe` Just (MarsRover (Position 1 2) N)
      parse rover "1 X N" `shouldBe` Nothing

    it "parses problem" $ do
      parse program problem `shouldBe` Just [(MarsRover (Position 1 2) N, [ILeft, IMove, ILeft, IMove, ILeft, IMove, ILeft, IMove, IMove])]

    it "moves the rover" $ do
      moveRover (MarsRover (Position 0 0) N) IMove `shouldBe` MarsRover (Position {x = 0, y = 1}) N
      moveRover (MarsRover (Position 0 0) S) IMove `shouldBe` MarsRover (Position {x = 0, y = -1}) S
      moveRover (MarsRover (Position 0 0) E) IMove `shouldBe` MarsRover (Position {x = 1, y = 0}) E
      moveRover (MarsRover (Position 0 0) W) IMove `shouldBe` MarsRover (Position {x = -1, y = 0}) W
      moveRover (MarsRover (Position 0 0) N) IRight `shouldBe` MarsRover (Position {x = 0, y = 0}) E
      moveRover (MarsRover (Position 0 0) N) ILeft `shouldBe` MarsRover (Position {x = 0, y = 0}) W

    it "runs the many rovers across mars" $ do
      runRovers
        [ (MarsRover (Position 0 0) N, [IMove]),
          (MarsRover (Position 0 0) N, [ILeft, IMove])
        ]
        `shouldBe` [ MarsRover (Position {x = 0, y = 1}) N,
                     MarsRover (Position {x = -1, y = 0}) W
                   ]

    it "runs the examples" $ do
      runRovers <$> parseProgram problem
        `shouldBe` Just
          [ MarsRover (Position 1 3) N
          ]
