module RoverParser where

import Control.Applicative hiding (many)
import Data.Char
import Data.Functor
import RoverTypes
import Text.ParserCombinators.ReadP

orientation :: ReadP Orientation
orientation =
  (char 'E' $> E)
    <|> (char 'N' $> N)
    <|> (char 'S' $> S)
    <|> (char 'W' $> W)

coordinates :: ReadP Pos
coordinates = Position <$> number <*> (space *> number)
  where
    number = read <$> munch1 isDigit
    space = char ' '

instruction :: ReadP Instruction
instruction =
  (char 'M' $> IMove)
    <|> (char 'L' $> ILeft)
    <|> (char 'R' $> IRight)

rover :: ReadP Rover
rover = MarsRover <$> coordinates <*> (char ' ' *> orientation)

program :: ReadP Program
program = coordinates >> char '\n' >> many coordAndInstr <* eof
  where
    parseInstructions = many instruction
    coordAndInstr = (,) <$> (rover <* char '\n') <*> (parseInstructions <* char '\n')

parseProgram :: String -> Maybe Program
parseProgram s = case x of
  [(x, _)] -> Just x
  _ -> Nothing
  where
    x = readP_to_S program s
