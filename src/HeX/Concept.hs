module HeX.Concept where

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
     deriving (Show)

class Dimensioned a where
    naturalLength :: TypoDim -> a -> Int

naturalWidth, naturalHeight, naturalDepth :: Dimensioned a => a -> Int
naturalWidth  = naturalLength Width
naturalHeight = naturalLength Height
naturalDepth  = naturalLength Depth

axisNaturalSpan :: Dimensioned a => Axis -> a -> Int
axisNaturalSpan Vertical   a = naturalHeight a + naturalDepth a
axisNaturalSpan Horizontal a = naturalWidth a
