{-# LANGUAGE DeriveFunctor #-}

module RoverTypes where

import Data.Bifunctor

data MarsRover a b = MarsRover
  { pos :: a,
    dir :: b
  }
  deriving (Show, Eq, Functor)

type Rover = MarsRover Pos Orientation

type Program = [(Rover, [Instruction])]

instance Bifunctor MarsRover where
  first f (MarsRover p d) = MarsRover (f p) d
  second = fmap

class Rotatable a where
  rotateToLeft :: a -> a
  rotateToRight :: a -> a

instance (Rotatable b) => Rotatable (MarsRover a b) where
  rotateToLeft = fmap rotateToLeft
  rotateToRight = fmap rotateToRight

data Orientation = N | E | S | W
  deriving (Show, Eq, Enum)

instance Rotatable Orientation where
  rotateToLeft N = W
  rotateToLeft W = S
  rotateToLeft S = E
  rotateToLeft E = N
  rotateToRight = rotateToLeft . rotateToLeft . rotateToLeft

data Instruction = IMove | IRight | ILeft
  deriving (Show, Eq)

data Position a = Position {x :: a, y :: a}
  deriving (Show, Eq)

type Pos = Position Int

class Movable a where
  moveUp :: a -> a
  moveDown :: a -> a
  moveLeft :: a -> a
  moveRight :: a -> a

instance (Num a) => Movable (Position a) where
  moveUp (Position x y) = Position x (y + 1)
  moveDown (Position x y) = Position x (y - 1)
  moveLeft (Position x y) = Position (x - 1) y
  moveRight (Position x y) = Position (x + 1) y

instance (Movable a) => Movable (MarsRover a b) where
  moveUp = first moveUp
  moveDown = first moveDown
  moveLeft = first moveLeft
  moveRight = first moveRight
