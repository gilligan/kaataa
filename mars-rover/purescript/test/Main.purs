module Test.Main where

import Prelude

import Rover (advanceRover, rotateRoverRight, rotateRoverLeft, mkRover, exec, run, Dir(..), Instruction(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    describe "Rover" do
       describe "advanceRover" do
          it "moves north" do
             advanceRover (mkRover 0 0 N) `shouldEqual` mkRover 0 1 N
             advanceRover (mkRover 0 0 S) `shouldEqual` mkRover 0 (-1) S
             advanceRover (mkRover 0 0 E) `shouldEqual` mkRover 1 0 E
             advanceRover (mkRover 0 0 W) `shouldEqual` mkRover (-1) 0 W
       describe "rotateRoverRight" do
          it "rotates the rover right" do
             rotateRoverRight (mkRover 0 0 N) `shouldEqual` (mkRover 0 0 E)
             rotateRoverRight (mkRover 0 0 S) `shouldEqual` (mkRover 0 0 W)
             rotateRoverRight (mkRover 0 0 E) `shouldEqual` (mkRover 0 0 S)
             rotateRoverRight (mkRover 0 0 W) `shouldEqual` (mkRover 0 0 N)
       describe "rotateRoverLeft" do
          it "rotates the rover left" do
             rotateRoverLeft (mkRover 0 0 N) `shouldEqual` (mkRover 0 0 W)
             rotateRoverLeft (mkRover 0 0 S) `shouldEqual` (mkRover 0 0 E)
             rotateRoverLeft (mkRover 0 0 E) `shouldEqual` (mkRover 0 0 N)
             rotateRoverLeft (mkRover 0 0 W) `shouldEqual` (mkRover 0 0 S)
       describe "exec" do
          it "executes a single Instruction" do
             exec (mkRover 0 0 N) M `shouldEqual` (mkRover 0 1 N)
             exec (mkRover 0 0 N) L `shouldEqual` (mkRover 0 0 W)
             exec (mkRover 0 0 N) R `shouldEqual` (mkRover 0 0 E)
       describe "run" do
          it "executes a sequence of instructions" do
             run (mkRover 0 0 N) [M, M] `shouldEqual` (mkRover 0 2 N)
             run (mkRover 0 0 N) [M, L, M, R] `shouldEqual` (mkRover (-1) 1 N)
