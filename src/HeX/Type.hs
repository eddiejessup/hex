module HeX.Type where

import HeXlude

import           Control.Applicative           ( Alternative
                                               , empty
                                               )
import           GHC.Generics                  ( Generic )

type IntVal = Int
type LenVal = Int

newNBitInt :: Alternative f => (Int -> a) -> Int ->  Int -> f a
newNBitInt f nBits n
    | n < 0 = empty
    | n >= (2 ^ nBits) = empty
    | otherwise = pure $ f n

-- 8-bit.

newtype EightBitInt = EightBitInt IntVal
    deriving (Show, Eq, Generic, Enum)

instance Hashable EightBitInt

instance Bounded EightBitInt where
    minBound = EightBitInt 0
    maxBound = EightBitInt (2 ^ (8 :: Int) - 1)

newEightBitInt :: Alternative f => Int -> f EightBitInt
newEightBitInt = newNBitInt EightBitInt 8

-- 4-bit.

newtype FourBitInt = FourBitInt IntVal
    deriving (Show, Eq, Generic, Enum)

instance Hashable FourBitInt

instance Bounded FourBitInt where
    minBound = FourBitInt 0
    maxBound = FourBitInt (2 ^ (4 :: Int) - 1)

newFourBitInt :: Alternative f => Int -> f FourBitInt
newFourBitInt = newNBitInt FourBitInt 4

-- Concepts.

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
