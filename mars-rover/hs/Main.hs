module Main where

import Rover (runRovers)
import RoverParser (parseProgram)

main :: IO ()
main =
  readFile "input.txt" >>= (putStrLn . maybeRun) . parseProgram
  where
    maybeRun = maybe "Invalid input" (show . runRovers)
