module Rover where

import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP
import Data.Char


data Orientation = N | S | E | W
   deriving (Show, Eq)

data Instruction = IMove | IRight | ILeft
   deriving (Show, Eq)

data Coord = Coord { x :: Int, y :: Int }
   deriving (Show, Eq)

data Rover = Rover Coord Orientation
   deriving (Show, Eq)


orientationReader :: ReadP Orientation
orientationReader = do
  c <- char 'E' <|> char 'N'  <|> char 'S'  <|> char 'W'
  return $ case c of
    'E' -> E
    'N' -> N
    'S' -> S
    'W' -> W

coordinateReader :: ReadP Coord
coordinateReader = do
 x <- number
 space
 y <- number
 return $ Coord (read x) (read y)
  where
   number = munch1 isDigit
   space = char ' '

instructionReader :: ReadP Instruction
instructionReader = do
  c <- char 'M' <|> char 'R'  <|> char 'L'
  return $ case c of
    'M' -> IMove
    'L' -> ILeft
    'R' -> IRight

-- sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]

parseRover :: ReadP Rover
parseRover = do
  coords <- coordinateReader
  char ' '
  orientation <- orientationReader
  return $ Rover coords orientation

parseInstructions :: ReadP [Instruction]
parseInstructions = many instructionReader


parseProblem :: ReadP [(Rover, [Instruction])]
parseProblem = do
  _ <- coordinateReader
  char '\n'
  many $ (do
    r <- parseRover
    char '\n'
    i <- parseInstructions
    char '\n'
    pure (r, i))
  <* eof

moveRover :: Rover -> Instruction -> Rover
moveRover (Rover c o) ILeft = Rover c (case o of N -> W; W -> S; S -> E; E -> N)
moveRover (Rover c o) IRight = Rover c (case o of N -> E; E -> S; S -> W; W -> N)
moveRover (Rover c o) IMove = Rover (case o of
                                      N -> c { y = y c + 1 }
                                      S -> c { y = y c - 1 }
                                      E -> c { x = x c + 1 }
                                      W -> c { x = x c - 1 }
                                    ) o

runRover :: Rover -> [Instruction] -> Rover
runRover = foldl moveRover
