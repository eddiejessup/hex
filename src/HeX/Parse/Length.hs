{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Length where

import qualified Text.Megaparsec               as P

import           HeX.Unit                       ( PhysicalUnit(..) )
import           HeX.Parse.AST
import           HeX.Parse.Common
import           HeX.Parse.Number
import           HeX.Parse.Stream

parseLength :: SimpExpandParser Length
parseLength = Length <$> parseSigns <*> parseUnsignedLength

parseUnsignedLength :: SimpExpandParser UnsignedLength
parseUnsignedLength = P.choice [ NormalLengthAsULength <$> parseNormalLength
                               -- , CoercedLength <$> parseCoercedLength
                               ]

parseNormalLength :: SimpExpandParser NormalLength
parseNormalLength = P.choice [ parseLengthSemiConstant
                             -- , InternalLength <$> parseInternalLength
                             ]
  where
    parseLengthSemiConstant = LengthSemiConstant <$> parseFactor <*> parseUnit

parseUnit :: SimpExpandParser Unit
parseUnit = P.choice [ parsePhysicalUnit
                     , parseInternalKeywordUnit
                     -- , parseInternalQuantityUnit
                     ]
  where
    parseInternalKeywordUnit =
        let parseUnitLit = P.choice $ P.try <$> [ parseKeywordToValue "em" Em
                                                , parseKeywordToValue "ex" Ex
                                                ]
        in  InternalUnit <$> parseUnitLit <* skipOneOptionalSpace

    parsePhysicalUnit :: SimpExpandParser Unit
    parsePhysicalUnit =
        -- TODO: Use 'try' because keywords with common prefixes lead the parser
        -- down a blind alley. Could refactor to avoid, but it would be ugly.
        -- Leave as later optimisation.
        -- TODO: Should we omit the last try in such cases?
        -- NOTE: Can't trim number of 'try's naïvely, because they all suck up
        -- initial space, which would also need backtracking.
        let parseUnitLit =
                P.choice $ P.try <$> [ parseKeywordToValue "bp" BigPoint
                                     , parseKeywordToValue "cc" Cicero
                                     , parseKeywordToValue "cm" Centimetre
                                     , parseKeywordToValue "dd" Didot
                                     , parseKeywordToValue "in" Inch
                                     , parseKeywordToValue "mm" Millimetre
                                     , parseKeywordToValue "pc" Pica
                                     , parseKeywordToValue "pt" Point
                                     , parseKeywordToValue "sp" ScaledPoint
                                     ]
        in  (PhysicalUnit <$> parseFrame <*> parseUnitLit) <* skipOneOptionalSpace

    parseFrame =
        do
        isTrue <- parseOptionalKeyword "true"
        pure $ if isTrue then TrueFrame else MagnifiedFrame

parseFactor :: SimpExpandParser Factor
-- NOTE: The order matters here: The TeX grammar seems to be ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
-- I don't think this is just a matter of backtracking, because the grammar is
-- simply ambiguous.
parseFactor = P.choice [ RationalConstant <$> P.try parseRationalConstant
                       , NormalIntegerFactor <$> P.try parseNormalInteger
                       ]
