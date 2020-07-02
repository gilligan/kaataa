module Main where

import Rover(parseProblem, runRovers)

main :: IO ()
main = do
  problem <- parseProblem <$> readFile "input.txt"
  case problem of
    Nothing -> putStrLn "Invalid input"
    Just problem -> print $ runRovers problem
