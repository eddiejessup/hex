{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Length where

import qualified Text.Megaparsec as P

import Unit (PhysicalUnit(..))

import Parse.Util (Parser)
import qualified Parse.Common as PC
import Parse.Number (NormalInteger, parseNormalInteger, parseRationalConstant, parseSigns)

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
  -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
  = LengthSemiConstant Factor Unit
  deriving Show

data Factor
  = NormalIntegerFactor NormalInteger
  -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
  -- with decimal digits, but its main feature is that it can represent
  -- non-integers.
  | RationalConstant Rational
  deriving Show

data Unit
  = InternalUnit InternalUnit
  -- true?
  | PhysicalUnit Bool PhysicalUnit
  deriving Show

data InternalUnit
  = Em
  | Ex
  -- | InternalIntegerUnit InternalInteger
  -- | InternalLengthUnit InternalLength
  -- | InternalGlueUnit InternalGlue
  deriving Show

-- data CoercedLength
--   = InternalGlueAsLength InternalGlue


-- Parse.

-- TODO:
-- - Internal quantity units

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
                             -- , InternalLength <$> parseInternalLength
                             ]
  where
    parseLengthSemiConstant = do
      factor <- parseFactor
      lengthUnit <- parseUnit
      return $ LengthSemiConstant factor lengthUnit

parseUnit :: Parser Unit
parseUnit = P.choice [ parsePhysicalUnit
                     , parseInternalKeywordUnit
                     -- , parseInternalQuantityUnit
                     ]
  where
    parseInternalKeywordUnit = do
      unit <- P.choice [ P.try $ PC.parseKeywordToValue "em" Em
                       , P.try $ PC.parseKeywordToValue "ex" Ex
                       ]
      PC.skipOneOptionalSpace
      return $ InternalUnit unit
    parsePhysicalUnit = do
      isTrue <- PC.parseOptionalKeyword "true"
      -- TODO: Use 'try' because keywords with common prefixes lead the parser
      -- down a blind alley. Could refactor to avoid, but it would be ugly.
      -- Leave as later optimisation.
      -- TODO: Should we omit the last try in such cases?
      -- NOTE: Can't trim number of 'try's naïvely, because they all suck up
      -- initial space, which would also need backtracking.
      unit <- P.choice [ P.try $ PC.parseKeywordToValue "bp" BigPoint
                       , P.try $ PC.parseKeywordToValue "cc" Cicero
                       , P.try $ PC.parseKeywordToValue "cm" Centimetre
                       , P.try $ PC.parseKeywordToValue "dd" Didot
                       , P.try $ PC.parseKeywordToValue "in" Inch
                       , P.try $ PC.parseKeywordToValue "mm" Millimetre
                       , P.try $ PC.parseKeywordToValue "pc" Pica
                       , P.try $ PC.parseKeywordToValue "pt" Point
                       , P.try $ PC.parseKeywordToValue "sp" ScaledPoint
                       ]
      PC.skipOneOptionalSpace
      return $ PhysicalUnit isTrue unit

parseFactor :: Parser Factor
-- NOTE: The order matters here: The TeX grammar seems to be ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
-- I don't think this is just a matter of backtracking, because the grammar is
-- simply ambiguous.
parseFactor = P.choice [ RationalConstant <$> P.try parseRationalConstant
                       , NormalIntegerFactor <$> P.try parseNormalInteger
                       ]
