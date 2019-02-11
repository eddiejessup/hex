{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HeX.Type where

import           Control.Applicative           ( Alternative
                                               , empty
                                               )
import           Data.Hashable
import           GHC.Generics (Generic)

type IntVal = Int
type LenVal = Int

newtype EightBitInt = EightBitInt IntVal
    deriving (Show, Eq, Generic, Enum)

instance Hashable EightBitInt

instance Bounded EightBitInt where
    minBound = EightBitInt 0
    maxBound = EightBitInt 255

newEightBitInt :: Alternative f => IntVal -> f EightBitInt
newEightBitInt n
    | n < 0 = empty
    | n > 255 = empty
    | otherwise = pure $ EightBitInt n

newtype ScaledPointNumber = ScaledPointNumber Int

newScaledPointNumber n
    | n < 0 = empty
    | otherwise = pure $ ScaledPointNumber n

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
