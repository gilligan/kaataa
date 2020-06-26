module Rover where

import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Functor


data Orientation = N | S | E | W
   deriving (Show, Eq)

data Instruction = IMove | IRight | ILeft
   deriving (Show, Eq)

data Coord = Coord { x :: Int, y :: Int }
   deriving (Show, Eq)

data Rover = Rover Coord Orientation
   deriving (Show, Eq)


orientationReader :: ReadP Orientation
orientationReader =
  (char 'E' $> E)
  <|>
  (char 'N' $> N)
  <|>
  (char 'S' $> S)
  <|>
  (char 'W' $> W)

coordinateReader :: ReadP Coord
coordinateReader = Coord <$> number <*> (space *> number)
  where
   number = read <$> munch1 isDigit
   space = char ' '

instructionReader :: ReadP Instruction
instructionReader =
  (char 'M' $> IMove)
  <|>
  (char 'L' $> ILeft)
  <|>
  (char 'R' $> IRight)

parseRover :: ReadP Rover
parseRover = Rover <$> coordinateReader <*> (char ' ' *> orientationReader)

parseInstructions :: ReadP [Instruction]
parseInstructions = many instructionReader


parseProblem :: ReadP [(Rover, [Instruction])]
parseProblem = coordinateReader >> char '\n' >> (many coordAndInstr) <* eof
 where
   coordAndInstr = (,) <$> (parseRover <* char '\n') <*> (parseInstructions <* char '\n')


rotateLeft :: Orientation -> Orientation
rotateLeft N = W
rotateLeft W = S
rotateLeft S = E
rotateLeft E = N

rotateRight :: Orientation -> Orientation
rotateRight N = E
rotateRight E = S
rotateRight S = W
rotateRight W = N

moveRover :: Rover -> Instruction -> Rover
moveRover (Rover c o) ILeft = Rover c $ rotateLeft o
moveRover (Rover c o) IRight = Rover c $ rotateRight o
moveRover (Rover c o) IMove = Rover (case o of
                                      N -> c { y = y c + 1 }
                                      S -> c { y = y c - 1 }
                                      E -> c { x = x c + 1 }
                                      W -> c { x = x c - 1 }
                                    ) o

runRover :: Rover -> [Instruction] -> Rover
runRover = foldl moveRover

runRovers :: [(Rover, [Instruction])] -> [Rover]
runRovers = fmap (uncurry runRover)
