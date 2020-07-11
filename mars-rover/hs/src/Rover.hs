module Rover where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.Functor
import System.IO
import Text.ParserCombinators.ReadP

data Orientation = N | S | E | W
  deriving (Show, Eq, Enum)

data Instruction = IMove | IRight | ILeft
  deriving (Show, Eq)

data Coord = Coord {x :: Int, y :: Int}
  deriving (Show, Eq)

data Rover = Rover {coord :: Coord, orientation :: Orientation}
  deriving (Show, Eq)

orientationReader :: ReadP Orientation
orientationReader =
  (char 'E' $> E)
    <|> (char 'N' $> N)
    <|> (char 'S' $> S)
    <|> (char 'W' $> W)

coordinateReader :: ReadP Coord
coordinateReader = Coord <$> number <*> (space *> number)
  where
    number = read <$> munch1 isDigit
    space = char ' '

instructionReader :: ReadP Instruction
instructionReader =
  (char 'M' $> IMove)
    <|> (char 'L' $> ILeft)
    <|> (char 'R' $> IRight)

parseRover :: ReadP Rover
parseRover = Rover <$> coordinateReader <*> (char ' ' *> orientationReader)

problemReader :: ReadP [(Rover, [Instruction])]
problemReader = coordinateReader >> char '\n' >> many coordAndInstr <* eof
  where
    parseInstructions = many instructionReader
    coordAndInstr = (,) <$> (parseRover <* char '\n') <*> (parseInstructions <* char '\n')

parseProblem :: String -> Maybe [(Rover, [Instruction])]
parseProblem s = case x of
  [(x, _)] -> Just x
  _ -> Nothing
  where
    x = readP_to_S problemReader s

parseProblem' :: String -> Maybe [(Rover, [Instruction])]
parseProblem' s =
  let res = readP_to_S problemReader s
   in case res of
        [(x, _)] -> Just x
        _ -> Nothing

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
moveRover (Rover c o) IMove =
  Rover
    ( case o of
        N -> c {y = y c + 1}
        S -> c {y = y c - 1}
        E -> c {x = x c + 1}
        W -> c {x = x c - 1}
    )
    o

runRover :: Rover -> [Instruction] -> Rover
runRover = foldl moveRover

runRovers :: [(Rover, [Instruction])] -> [Rover]
runRovers = fmap (uncurry runRover)

main :: IO ()
main = do
  problem <- parseProblem <$> readFile "input.txt"
  case problem of
    Nothing -> print "Invalid input"
    Just problem -> print $ runRovers problem
