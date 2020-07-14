module Rover where

import Prelude
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Dir = N | S | E | W
data Instruction = L | R | M

type Point = { x :: Int, y :: Int }
type Rover = { pos :: Point, dir :: Dir }

mkRover :: Int -> Int -> Dir -> Rover
mkRover x y d = { pos: { x: x, y: y }, dir: d }

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
