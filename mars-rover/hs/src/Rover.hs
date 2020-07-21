{-# LANGUAGE DerivingVia #-}

module Rover where

import RoverTypes

moveRover :: Rover -> Instruction -> Rover
moveRover r ILeft = rotateToLeft r
moveRover r IRight = rotateToRight r
moveRover r IMove = case dir r of
  N -> moveUp r
  S -> moveDown r
  E -> moveRight r
  W -> moveLeft r

runRovers :: Program -> [Rover]
runRovers = fmap (uncurry runRover)
  where
    runRover = foldl moveRover
