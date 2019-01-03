module Data.Concept where

data HDirection
  = Leftward
  | Rightward
  deriving (Show, Eq)

data VDirection
  = Upward
  | Downward
  deriving (Show, Eq)

data Direction
  = Forward
  | Backward
  deriving (Show, Eq)

data Axis
  = Horizontal
  | Vertical
  deriving (Show, Eq)

data MoveMode = Put | Set
  deriving Show
