module Parse.AST where

-- Numbers.

data Number
  = Number Bool UnsignedNumber
  deriving Show

data UnsignedNumber
  = NormalIntegerAsUNumber NormalInteger
  -- | CoercedInteger CoercedInteger
  deriving Show

data NormalInteger
  -- = InternalInteger InternalInteger
  = IntegerConstant Int
  deriving Show

-- data CoercedInteger
--   = InternalLengthAsInt InternalLength
--   | InternalGlueAsInt InternalGlue

-- Lengths.

data Length
  = Length Bool UnsignedLength
  deriving Show

data UnsignedLength
  = NormalLengthAsULength NormalLength
  -- | CoercedLength CoercedLength
  deriving Show

data NormalLength
  -- = InternalLength InternalLength
  = LengthSemiConstant Factor LengthUnit
  deriving Show

data Factor
  = NormalIntegerFactor NormalInteger
  | DecimalConstant Rational
  deriving Show

data LengthUnit
  = InternalLengthUnit InternalLengthUnit
  -- true?
  | PhysicalLengthUnit Bool PhysicalLengthUnit
  deriving Show

data InternalLengthUnit
  = Em
  | Ex
  -- | InternalIntegerLengthUnit InternalInteger
  -- | InternalLengthLengthUnit InternalLength
  -- | InternalGlueLengthUnit InternalGlue
  deriving Show

data PhysicalLengthUnit
  = Point
  | Pica
  | Inch
  | BigPoint
  | Centimetre
  | Millimetre
  | Didot
  | Cicero
  | ScaledPoint
  deriving Show

-- data CoercedLength
--   = InternalGlueAsLength InternalGlue

-- data InternalInteger = TODO
-- data InternalLength = TODO
-- data InternalGlue = TODO
