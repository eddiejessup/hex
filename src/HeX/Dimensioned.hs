module HeX.Dimensioned where

class Dimensioned a where
  naturalWidth, naturalHeight, naturalDepth :: a -> Int
