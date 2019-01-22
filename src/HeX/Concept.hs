module HeX.Concept where

import HeX.Type

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

data MoveMode
    = Put
    | Set
    deriving (Show)

data TypoDim
     = Width
     | Height
     | Depth
     deriving (Show, Eq)

class Dimensioned a where
    naturalLength :: TypoDim -> a -> LenVal

naturalWidth, naturalHeight, naturalDepth :: Dimensioned a => a -> LenVal
naturalWidth  = naturalLength Width
naturalHeight = naturalLength Height
naturalDepth  = naturalLength Depth

axisNaturalSpan :: Dimensioned a => Axis -> a -> LenVal
axisNaturalSpan Vertical   a = naturalHeight a + naturalDepth a
axisNaturalSpan Horizontal a = naturalWidth a
