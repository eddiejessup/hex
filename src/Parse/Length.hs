{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Length where

import qualified Text.Megaparsec as P

import Unit (PhysicalUnit(..))

import Parse.Util (Parser)
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

-- Think: 'un-coerced length'.
data NormalLength
  -- = InternalLength InternalLength
  -- semi-constant because Factor and Unit can be quite un-constant-like.
  = LengthSemiConstant Factor Unit
  deriving Show

data Factor
  = NormalIntegerFactor NormalInteger
  | DecimalConstant Rational
  deriving Show

data Unit
  = InternalUnit InternalUnit
  -- true?
  | PhysicalUnit Bool PhysicalUnit
  deriving Show

data InternalUnit
  = Em
  | Ex
  -- | InternalIntegerLengthUnit InternalInteger
  -- | InternalLengthLengthUnit InternalLength
  -- | InternalGlueLengthUnit InternalGlue
  deriving Show

-- data CoercedLength
--   = InternalGlueAsLength InternalGlue


-- Parse.

-- TODO:
-- - Decimal constant
-- - 'true' for physical units
-- - Physical units beyond 'pt'
-- - Keyword internal units
-- - Keyword internal units

parseLength :: Parser Length
parseLength = do
  pos <- parseSigns
  uLn <- parseUnsignedLength
  return $ Length pos uLn

parseUnsignedLength :: Parser UnsignedLength
parseUnsignedLength = P.choice [ NormalLengthAsULength <$> parseNormalLength
                               -- , CoercedLength <$> parseCoercedLength
                               ]

parseNormalLength :: Parser NormalLength
parseNormalLength = P.choice [ parseLengthSemiConstant
                             -- , parseInternalLengthAsNormalLength
                             ]
  where
    parseLengthSemiConstant = do
      factor <- parseFactor
      lengthUnit <- parseUnit
      return $ LengthSemiConstant factor lengthUnit

parseUnit :: Parser Unit
parseUnit = P.choice [ parsePhysicalLengthUnit
                     -- , parseInternalLengthUnitAsLengthUnit
                     ]
  where
    -- parseInternalLengthUnitAsLengthUnit = do
    --   arg <- parseInternalLengthUnit
    --   return $ InternalUnit arg
    parsePhysicalLengthUnit = do
      -- isTrue <- parseTrueKeyword
      let isTrue = False
      -- Use 'try' because keywords with common prefixes lead the parser
      -- down a blind alley. Could probably refactor to avoid, but it would be
      -- ugly. Leave optimisation for later.
      unit <- P.choice [ P.try $ PC.parseKeywordToValue "pt" Point
                       , P.try $ PC.parseKeywordToValue "pc" Pica
                       , P.try $ PC.parseKeywordToValue "in" Inch
                       , P.try $ PC.parseKeywordToValue "bp" BigPoint
                       , P.try $ PC.parseKeywordToValue "cm" Centimetre
                       , P.try $ PC.parseKeywordToValue "mm" Millimetre
                       , P.try $ PC.parseKeywordToValue "dd" Didot
                       , P.try $ PC.parseKeywordToValue "cc" Cicero
                       , P.try $ PC.parseKeywordToValue "sp" ScaledPoint
                       ]
      return $ PhysicalUnit isTrue unit

parseFactor :: Parser Factor
parseFactor = P.choice [ NormalIntegerFactor <$> parseNormalInteger
                       -- TODO: parseRationalConstant
                       ]
