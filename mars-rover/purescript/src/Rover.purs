module Rover where

import Prelude

import Control.Alternative ((<|>))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (List)
import Data.List.NonEmpty (NonEmptyList)

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many, many1, withError, (<?>))
import Text.Parsing.StringParser.CodePoints (string, char)

{-- Rover Code --}

data Dir = N | S | E | W
data Instruction = L | R | M

type Point = { x :: Int, y :: Int }
type Rover = { pos :: Point, dir :: Dir }
type RoverSpec = { rover :: Rover, instructions :: List Instruction }

mkRoverSpec :: Rover -> List Instruction -> RoverSpec
mkRoverSpec rover instructions = { rover, instructions }

mkRover :: Int -> Int -> Dir -> Rover
mkRover x y dir = { pos: { x, y }, dir }

derive instance eqInst :: Eq Instruction
derive instance genericInst :: Generic Instruction _
instance showInst :: Show Instruction
    where show = genericShow

derive instance eqDir :: Eq Dir
derive instance genericDir :: Generic Dir _
instance showDir :: Show Dir
    where show = genericShow

rotateLeft :: Dir -> Dir
rotateLeft N = W
rotateLeft W = S
rotateLeft S = E
rotateLeft E = N

rotateRight :: Dir -> Dir
rotateRight N = E
rotateRight E = S
rotateRight S = W
rotateRight W = N

exec :: Rover -> Instruction -> Rover
exec r L = rotateRoverLeft r
exec r R = rotateRoverRight r
exec r M = advanceRover r

run :: Rover -> Array Instruction -> Rover
run = foldl exec

rotateRoverLeft :: Rover -> Rover
rotateRoverLeft r@{ dir, pos } = r { dir = rotateLeft dir }

rotateRoverRight :: Rover -> Rover
rotateRoverRight r@{ dir, pos } = r { dir = rotateRight dir }

advanceRover :: Rover -> Rover
advanceRover r@{ dir, pos }  =
    let
        p' = case (r.dir) of
                  N -> pos { y = pos.y + 1 }
                  S -> pos { y = pos.y - 1 }
                  E -> pos { x = pos.x + 1 }
                  W -> pos { x = pos.x - 1 }
     in
        r { pos = p' }

{-- Parser Code --}

rover :: Parser Rover
rover = mkRover <$> integer <*> integer <*> direction

dimensions :: Parser (Array Int)
dimensions = do
    w <- integer
    _ <- char ' '
    h <- integer
    pure $ [w, h]

roverSpec :: Parser RoverSpec
roverSpec = mkRoverSpec <$> (rover <* char '\n') <*> instructions

program :: Parser (NonEmptyList RoverSpec)
program = do
    _ <- dimensions
    _ <- char '\n'
    many1 (roverSpec <* char '\n')

direction :: Parser Dir
direction = string "N" $> N
    <|> string "S" $> S
    <|> string "E" $> E
    <|> string "W" $> W

instruction :: Parser Instruction
instruction = string "L" $> L
    <|> string "R" $> R
    <|> string "M" $> M

instructions :: Parser (List Instruction)
instructions = many instruction

digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9

natural :: Parser Int
natural = flip withError "Expected natural" $ do
    foldl (\acc n -> (acc * 10) + n) 0 <$> many1 digit

integer :: Parser Int
integer = (signParser <*> natural) <?> "Expected integer"

signParser :: Parser (Int -> Int)
signParser = ((string "-") *> pure negate) <|> pure identity
