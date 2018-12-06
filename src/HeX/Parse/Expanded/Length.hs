{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Expanded.Length where

import qualified Text.Megaparsec               as P

import           HeX.Unit                       ( PhysicalUnit(..) )

import           HeX.Parse.Expanded.Common
import           HeX.Parse.Expanded.Number
import           HeX.Parse.Expanded.Stream

-- AST.
data Length =
  Length Bool
         UnsignedLength
  deriving (Show)

data UnsignedLength =
  NormalLengthAsULength NormalLength
  -- \| CoercedLength CoercedLength
  deriving (Show)

-- Think: 'un-coerced length'.
data NormalLength
  -- = InternalLength InternalLength
  -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
      =
  LengthSemiConstant Factor
                     Unit
  deriving (Show)

data Factor
  = NormalIntegerFactor NormalInteger
  -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
  -- with decimal digits, but its main feature is that it can represent
  -- non-integers.
  | RationalConstant Rational
  deriving (Show)

data Unit
  = InternalUnit InternalUnit
  -- true?
  | PhysicalUnit Bool
                 PhysicalUnit
  deriving (Show)

data InternalUnit
  = Em
  | Ex
  -- \| InternalIntegerUnit InternalInteger
  -- \| InternalLengthUnit InternalLength
  -- \| InternalGlueUnit InternalGlue
  deriving (Show)

-- data CoercedLength
--   = InternalGlueAsLength InternalGlue
-- Parse.
-- TODO:
-- - Internal quantity units
parseLength :: SimpExpandParser Length
parseLength = Length <$> parseSigns <*> parseUnsignedLength

parseUnsignedLength :: SimpExpandParser UnsignedLength
parseUnsignedLength =
  P.choice
    [ NormalLengthAsULength <$> parseNormalLength
                               -- , CoercedLength <$> parseCoercedLength
    ]

parseNormalLength :: SimpExpandParser NormalLength
parseNormalLength =
  P.choice
    [ parseLengthSemiConstant
                             -- , InternalLength <$> parseInternalLength
    ]
  where
    parseLengthSemiConstant = LengthSemiConstant <$> parseFactor <*> parseUnit

parseUnit :: SimpExpandParser Unit
parseUnit =
  P.choice
    [ parsePhysicalUnit
    , parseInternalKeywordUnit
                     -- , parseInternalQuantityUnit
    ]
  where
    parseInternalKeywordUnit =
      let parseUnitLit =
            P.choice
              [ P.try $ parseKeywordToValue "em" Em
              , P.try $ parseKeywordToValue "ex" Ex
              ]
      in InternalUnit <$> parseUnitLit <* skipOneOptionalSpace
    parsePhysicalUnit
      -- TODO: Use 'try' because keywords with common prefixes lead the parser
      -- down a blind alley. Could refactor to avoid, but it would be ugly.
      -- Leave as later optimisation.
      -- TODO: Should we omit the last try in such cases?
      -- NOTE: Can't trim number of 'try's naïvely, because they all suck up
      -- initial space, which would also need backtracking.
     =
      let parseUnitLit =
            P.choice
              [ P.try $ parseKeywordToValue "bp" BigPoint
              , P.try $ parseKeywordToValue "cc" Cicero
              , P.try $ parseKeywordToValue "cm" Centimetre
              , P.try $ parseKeywordToValue "dd" Didot
              , P.try $ parseKeywordToValue "in" Inch
              , P.try $ parseKeywordToValue "mm" Millimetre
              , P.try $ parseKeywordToValue "pc" Pica
              , P.try $ parseKeywordToValue "pt" Point
              , P.try $ parseKeywordToValue "sp" ScaledPoint
              ]
      in (PhysicalUnit <$> parseOptionalKeyword "true" <*> parseUnitLit) <*
         skipOneOptionalSpace

parseFactor :: SimpExpandParser Factor
-- NOTE: The order matters here: The TeX grammar seems to be ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
-- I don't think this is just a matter of backtracking, because the grammar is
-- simply ambiguous.
parseFactor =
  P.choice
    [ RationalConstant <$> P.try parseRationalConstant
    , NormalIntegerFactor <$> P.try parseNormalInteger
    ]
