{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Length where

import qualified Text.Megaparsec as P

import qualified Parse.Common as PC
import Parse.Number (NormalInteger, parseNormalInteger, parseSigns)


-- AST.

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


-- Parse.

parseLength = do
  pos <- parseSigns
  uLn <- parseUnsignedLength
  return $ Length pos uLn

parseUnsignedLength = P.choice [ NormalLengthAsULength <$> parseNormalLength
                               -- , CoercedLength <$> parseCoercedLength
                               ]

parseNormalLength = P.choice [ parseLengthSemiConstant
                             -- , parseInternalLengthAsNormalLength
                             ]
  where
    parseLengthSemiConstant = do
      factor <- parseFactor
      lengthUnit <- parseLengthUnit
      return $ LengthSemiConstant factor lengthUnit

parseLengthUnit = P.choice [ parsePhysicalLengthUnit
                           -- , parseInternalLengthUnitAsLengthUnit
                           ]
  where
    -- parseInternalLengthUnitAsLengthUnit = do
    --   arg <- parseInternalLengthUnit
    --   return $ InternalLengthUnit arg
    parsePhysicalLengthUnit = do
      -- isTrue <- parseTrueKeyword
      let isTrue = False
      unit <- P.choice [ parsePt ]
      return $ PhysicalLengthUnit isTrue unit

parsePt = do
  PC.skipKeyword "pt"
  return Point

parseFactor = P.choice [ NormalIntegerFactor <$> parseNormalInteger
                       -- TODO: parseRationalConstant
                       ]
