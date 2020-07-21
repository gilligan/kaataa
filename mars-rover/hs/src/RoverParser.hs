module RoverParser where

import Control.Applicative hiding (many)
import Data.Char
import Data.Functor
import RoverTypes
import Text.ParserCombinators.ReadP

orientationReader :: ReadP Orientation
orientationReader =
  (char 'E' $> E)
    <|> (char 'N' $> N)
    <|> (char 'S' $> S)
    <|> (char 'W' $> W)

coordinateReader :: ReadP Pos
coordinateReader = Position <$> number <*> (space *> number)
  where
    number = read <$> munch1 isDigit
    space = char ' '

instructionReader :: ReadP Instruction
instructionReader =
  (char 'M' $> IMove)
    <|> (char 'L' $> ILeft)
    <|> (char 'R' $> IRight)

parseRover :: ReadP Rover
parseRover = MarsRover <$> coordinateReader <*> (char ' ' *> orientationReader)

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
