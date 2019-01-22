{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module HeX.Parse.Number where

import           Data.Foldable                  ( foldl' )
import           Data.Ratio                     ( (%) )
import           Data.Functor                   ( ($>) )

import qualified Text.Megaparsec               as P

import qualified HeX.Lex                       as Lex
import           HeX.Unit                       ( PhysicalUnit(..) )
import           HeX.Parse.Helpers
import           HeX.Parse.AST
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Stream

-- Number.

parseNumber :: SimpExpandParser Number
parseNumber = Number <$> parseSigns <*> parseUnsignedNumber

parseSigns :: SimpExpandParser Sign
parseSigns = mconcat <$> parseOptionalSigns
  where
    parseOptionalSigns =
        skipOptionalSpaces *> P.sepEndBy (satisfyThen signToPos) skipOptionalSpaces

    signToPos (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat '+' Lex.Other))) =
        Just $ Sign True
    signToPos (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat '-' Lex.Other))) =
        Just $ Sign False
    signToPos _ =
        Nothing

parseUnsignedNumber :: SimpExpandParser UnsignedNumber
parseUnsignedNumber = P.choice [ NormalIntegerAsUNumber <$> parseNormalInteger
                               , CoercedInteger <$> parseCoercedInteger
                               ]

-- Restrict pure type, and therefore accumulator, to Integer, to disallow
-- overflow.
digitsToInteger :: Integral n => n -> [n] -> Integer
digitsToInteger base = foldl' (\a b -> a * fromIntegral base + fromIntegral b) 0

parseNormalInteger :: SimpExpandParser NormalInteger
parseNormalInteger =
    P.choice [ InternalInteger <$> parseInternalInteger
             , IntegerConstant <$> parseConstantInt <* skipOneOptionalSpace
             ]
  where
    parseConstantInt = P.choice [ parseConstant, parseCharacter ]

    parseConstant =
        do
        (digits, base) <- P.choice [ (, 10) <$> P.some parseDecimalIntegerDigit
                                   , (, 16) <$> parseHexadecimalIntegerDigits
                                   , (, 8) <$> parseOctalIntegerDigits
                                   ]
        pure $ fromIntegral $ digitsToInteger base digits

    parseCharacter = skipSatisfied isBacktick *> parseCharLike

    isBacktick (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat '`' Lex.Other))) = True
    isBacktick _ = False

parseDecimalIntegerDigit :: SimpExpandParser Int
parseDecimalIntegerDigit = satisfyThen decCharToInt
  where
    decCharToInt (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) =
        case c of
            '0' -> Just 0
            '1' -> Just 1
            '2' -> Just 2
            '3' -> Just 3
            '4' -> Just 4
            '5' -> Just 5
            '6' -> Just 6
            '7' -> Just 7
            '8' -> Just 8
            '9' -> Just 9
            _   -> Nothing
    decCharToInt _ = Nothing

parseHexadecimalIntegerDigits :: SimpExpandParser [Int]
parseHexadecimalIntegerDigits =
    skipSatisfied isDoubleQuote *> P.some (satisfyThen hexCharToInt)
  where
    isDoubleQuote (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat '"' Lex.Other))) = True
    isDoubleQuote _ = False

    hexCharToInt (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) =
        case c of
            '0' -> Just 0
            '1' -> Just 1
            '2' -> Just 2
            '3' -> Just 3
            '4' -> Just 4
            '5' -> Just 5
            '6' -> Just 6
            '7' -> Just 7
            '8' -> Just 8
            '9' -> Just 9
            'A' -> Just 10
            'B' -> Just 11
            'C' -> Just 12
            'D' -> Just 13
            'E' -> Just 14
            'F' -> Just 15
            _   -> Nothing
    hexCharToInt (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter))) =
        case c of
            'A' -> Just 10
            'B' -> Just 11
            'C' -> Just 12
            'D' -> Just 13
            'E' -> Just 14
            'F' -> Just 15
            _   -> Nothing
    hexCharToInt _ = Nothing

parseOctalIntegerDigits :: SimpExpandParser [Int]
parseOctalIntegerDigits =
    skipSatisfied isSingleQuote *> P.some (satisfyThen octCharToInt)
  where
    isSingleQuote (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat '\'' Lex.Other))) = True
    isSingleQuote _ = False

    octCharToInt (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) =
        case c of
            '0' -> Just 0
            '1' -> Just 1
            '2' -> Just 2
            '3' -> Just 3
            '4' -> Just 4
            '5' -> Just 5
            '6' -> Just 6
            '7' -> Just 7
            _   -> Nothing
    octCharToInt _ = Nothing

parseCoercedInteger :: SimpExpandParser CoercedInteger
parseCoercedInteger = P.choice [ InternalLengthAsInt <$> parseInternalLength
                               , InternalGlueAsInt <$> parseInternalGlue
                               ]

-- Length.

parseLength :: SimpExpandParser Length
parseLength = Length <$> parseSigns <*> parseUnsignedLength

parseUnsignedLength :: SimpExpandParser UnsignedLength
parseUnsignedLength = P.choice [ NormalLengthAsULength <$> parseNormalLength
                               , CoercedLength <$> parseCoercedLength
                               ]

parseNormalLength :: SimpExpandParser NormalLength
parseNormalLength = P.choice [ LengthSemiConstant <$> parseFactor <*> parseUnit
                             , InternalLength <$> parseInternalLength
                             ]

parseFactor :: SimpExpandParser Factor
-- NOTE: The order matters here: The TeX grammar seems to be ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
-- I don't think this is just a matter of backtracking, because the grammar is
-- simply ambiguous.
parseFactor = P.choice $ P.try <$> [ NormalIntegerFactor <$> parseNormalInteger
                                   , RationalConstant <$> parseRationalConstant
                                   ]

parseRationalConstant :: SimpExpandParser Rational
parseRationalConstant =
    do
    wholeNr <- decDigitsToInteger <$> P.many parseDecimalIntegerDigit
    skipSatisfied isDotOrComma
    -- The fractional part represents its integer interpretation, divided by
    -- the next largest power of 10.
    -- TODO: If performance matters, maybe we can infer the denominator
    -- faster from the integer itself than the digits.
    fracDigits <- P.many parseDecimalIntegerDigit
    let fraction =
            fromIntegral (decDigitsToInteger fracDigits) % (10 ^ length fracDigits)
    -- Convert the whole number to a rational, and add it to the fraction.
    pure $ fromIntegral wholeNr + fraction
  where
    decDigitsToInteger = digitsToInteger 10

    isDotOrComma (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat ',' Lex.Other))) = True
    isDotOrComma (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat '.' Lex.Other))) = True
    isDotOrComma _ = False

parseUnit :: SimpExpandParser Unit
parseUnit = P.choice [ skipOptionalSpaces *> (InternalUnit <$> parseInternalUnit)
                     , (PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* skipOneOptionalSpace
                     ]
  where
    parseInternalUnit = P.choice [ parseInternalUnitLit <* skipOneOptionalSpace
                                 , InternalIntegerUnit <$> parseInternalInteger
                                 , InternalLengthUnit <$> parseInternalLength
                                 , InternalGlueUnit <$> parseInternalGlue
                                 ]

    parseInternalUnitLit = P.choice $ P.try <$> [ parseKeywordToValue "em" Em
                                                , parseKeywordToValue "ex" Ex
                                                ]

    -- TODO: Use 'try' because keywords with common prefixes lead the parser
    -- down a blind alley. Could refactor to avoid, but it would be ugly.
    -- Leave as later optimisation.
    -- TODO: Should we omit the last try in such cases?
    -- NOTE: Can't trim number of 'try's naïvely, because they all suck up
    -- initial space, which would also need backtracking.
    parsePhysicalUnitLit = P.choice $ P.try <$> [ parseKeywordToValue "bp" BigPoint
                                                , parseKeywordToValue "cc" Cicero
                                                , parseKeywordToValue "cm" Centimetre
                                                , parseKeywordToValue "dd" Didot
                                                , parseKeywordToValue "in" Inch
                                                , parseKeywordToValue "mm" Millimetre
                                                , parseKeywordToValue "pc" Pica
                                                , parseKeywordToValue "pt" Point
                                                , parseKeywordToValue "sp" ScaledPoint
                                                ]

    parseFrame =
        do
        isTrue <- parseOptionalKeyword "true"
        pure $ if isTrue then TrueFrame else MagnifiedFrame

parseCoercedLength = InternalGlueAsLength <$> parseInternalGlue

-- Math length.

parseMathLength = MathLength <$> parseSigns <*> parseUnsignedMathLength

parseUnsignedMathLength :: SimpExpandParser UnsignedMathLength
parseUnsignedMathLength = P.choice [ NormalMathLengthAsUMathLength <$> parseNormalMathLength
                                   , CoercedMathLength <$> parseCoercedMathLength
                                   ]

parseNormalMathLength :: SimpExpandParser NormalMathLength
parseNormalMathLength = MathLengthSemiConstant <$> parseFactor <*> parseMathUnit

parseMathUnit :: SimpExpandParser MathUnit
parseMathUnit = P.choice [ skipKeyword "mu" >> skipOneOptionalSpace $> Mu
                         , skipOptionalSpaces *> (InternalMathGlueAsUnit <$> parseInternalMathGlue)
                         ]

parseCoercedMathLength = InternalMathGlueAsMathLength <$> parseInternalMathGlue

-- Glue.

parseGlue :: SimpExpandParser Glue
parseGlue = P.choice [ ExplicitGlue <$> parseLength <*> parseFlex "plus" <*> parseFlex "minus"
                     , InternalGlue <$> parseSigns <*> parseInternalGlue
                     ]

parseFlex :: String -> SimpExpandParser (Maybe Flex)
parseFlex s = P.choice [ Just <$> P.try parsePresentFlex
                       , const Nothing <$> skipOptionalSpaces
                       ]
  where
    parsePresentFlex = skipKeyword s *> (P.choice $ P.try <$> [ FiniteFlex <$> parseLength
                                                              , FilFlex <$> parseFilLength
                                                              ])

parseFilLength :: SimpExpandParser FilLength
parseFilLength =
    (FilLength <$> parseSigns <*> parseFactor <*> parseOrder) <* skipOptionalSpaces
  where
    parseSomeLs = P.some $ skipSatisfied $ matchNonActiveCharacterUncased 'l'

    parseOrder = skipKeyword "fi" *> (length <$> parseSomeLs)

-- Math glue.

parseMathGlue :: SimpExpandParser MathGlue
parseMathGlue = P.choice [ ExplicitMathGlue <$> parseMathLength <*> parseMathFlex "plus" <*> parseMathFlex "minus"
                         , InternalMathGlue <$> parseSigns <*> parseInternalMathGlue
                         ]

parseMathFlex :: String -> SimpExpandParser (Maybe MathFlex)
parseMathFlex s = P.choice [ Just <$> P.try parsePresentFlex
                           , const Nothing <$> skipOptionalSpaces
                           ]
  where
    parsePresentFlex = skipKeyword s *> (P.choice $ P.try <$> [ FiniteMathFlex <$> parseMathLength
                                                              , FilMathFlex <$> parseFilLength
                                                              ])

-- Internal quantities.

parseQuantityVariable
    :: (T.PrimitiveToken -> Maybe p) -- Try to extract a parameter from a token.
    -> (T.PrimitiveToken -> Maybe v) -- Try to extract a short-def token value from a token.
    -> T.PrimitiveToken -- A token signifying the start of a relevant register address.
    -> SimpExpandParser (QuantVariable p v)
parseQuantityVariable getParam getTok regHead =
    P.choice [ ParamVar <$> satisfyThen getParam
             , TokenVar <$> satisfyThen getTok
             , RegisterVar <$> (skipSatisfiedEquals regHead >> parseNumber)
             ]

parseIntegerVariable :: SimpExpandParser IntegerVariable
parseIntegerVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegInt)
  where
    getParam (T.IntParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.IntToken s) = Just s
    getTok _ = Nothing

parseLengthVariable :: SimpExpandParser LengthVariable
parseLengthVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegLen)
  where
    getParam (T.LenParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.LenToken s) = Just s
    getTok _ = Nothing

parseGlueVariable :: SimpExpandParser GlueVariable
parseGlueVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegGlue)
  where
    getParam (T.GlueParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.GlueToken s) = Just s
    getTok _ = Nothing

parseMathGlueVariable :: SimpExpandParser MathGlueVariable
parseMathGlueVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegMathGlue)
  where
    getParam (T.MathGlueParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.MathGlueToken s) = Just s
    getTok _ = Nothing

parseTokenListVariable :: SimpExpandParser TokenListVariable
parseTokenListVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegTokenList)
  where
    getParam (T.TokenListParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.TokenListToken s) = Just s
    getTok _ = Nothing

parseInternalInteger = P.choice [ InternalIntegerVariable <$> parseIntegerVariable
                                , InternalSpecialInteger <$> parseSpecialInteger
                                , InternalCodeTableRef <$> parseCodeTableRef
                                , InternalCharToken <$> parseCharToken
                                , InternalMathCharToken <$> parseMathCharToken
                                , InternalFontCharRef <$> parseFontCharRef
                                , skipSatisfiedEquals T.LastPenaltyTok $> LastPenalty
                                , skipSatisfiedEquals T.ParagraphShapeTok $> ParShape
                                , skipSatisfiedEquals T.InputLineNrTok $> InputLineNr
                                , skipSatisfiedEquals T.BadnessTok $> Badness
                                ]

parseCharToken = satisfyThen (\case
    (T.CharToken c) -> Just c
    _               -> Nothing)

parseMathCharToken = satisfyThen (\case
    (T.MathCharToken c) -> Just c
    _                   -> Nothing)

parseSpecialInteger = satisfyThen (\case
    T.SpecialIntegerTok p -> Just p
    _                     -> Nothing)

parseCodeTableRef = CodeTableRef <$> satisfyThen tokToCodeType <*> parseNumber
  where
    tokToCodeType (T.CodeTypeTok c) = Just c
    tokToCodeType _               = Nothing

parseFontCharRef = FontCharRef <$> satisfyThen tokToFontChar <*> parseFontRef
  where
    tokToFontChar (T.FontCharTok c) = Just c
    tokToFontChar _                 = Nothing

parseFontRef = P.choice [ FontTokenRef <$> parseFontRefToken
                        , skipSatisfiedEquals T.FontTok $> CurrentFontRef
                        , FamilyMemberFontRef <$> parseFamilyMember
                        ]

parseFontRefToken = satisfyThen (\case
    T.FontRefToken n -> Just n
    _                -> Nothing)

parseFamilyMember = FamilyMember <$> (satisfyThen tokToFontRange) <*> parseNumber
  where
    tokToFontRange (T.FontRangeTok r) = Just r
    tokToFontRange _                  = Nothing

parseInternalLength :: SimpExpandParser InternalLength
parseInternalLength = P.choice [ InternalLengthVariable <$> parseLengthVariable
                               , InternalSpecialLength <$> parseSpecialLength
                               , InternalFontDimensionRef <$> parseFontDimensionRef
                               , InternalBoxDimensionRef <$> parseBoxDimensionRef
                               , skipSatisfiedEquals T.LastKernTok $> LastKern
                               ]

parseSpecialLength = satisfyThen (\case
    T.SpecialLengthTok p -> Just p
    _                    -> Nothing)

parseFontDimensionRef = skipSatisfiedEquals T.FontDimensionTok >> (FontDimensionRef <$> parseNumber <*> parseFontRef)

parseBoxDimensionRef = do
    dim <- parseBoxDimension
    boxNr <- parseNumber
    pure $ BoxDimensionRef boxNr dim

parseBoxDimension = satisfyThen (\case
    (T.BoxDimensionTok d) -> Just d
    _                     -> Nothing)

parseInternalGlue = P.choice [ InternalGlueVariable <$> parseGlueVariable
                             , skipSatisfiedEquals T.LastGlueTok $> LastGlue
                             ]

parseInternalMathGlue = P.choice [ InternalMathGlueVariable <$> parseMathGlueVariable
                                 , skipSatisfiedEquals T.LastGlueTok $> LastMathGlue
                                 ]
