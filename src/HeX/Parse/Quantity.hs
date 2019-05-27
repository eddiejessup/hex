module HeX.Parse.Quantity where

import           HeXlude

import           Data.Foldable       ( foldl' )
import           Data.Functor        ( ($>) )
import           Data.Ratio          ( (%) )

import qualified Text.Megaparsec     as P

import           HeX.Categorise      ( CharCode )
import qualified HeX.Lex             as Lex
import           HeX.Parse.AST
import           HeX.Parse.Common
import           HeX.Parse.Helpers
import           HeX.Parse.Inhibited
import qualified HeX.Parse.Token     as T
import           HeX.Unit            ( PhysicalUnit(..) )

-- TeXInt.

parseTeXInt :: InhibitableStream s => SimpParser s TeXInt
parseTeXInt = TeXInt <$> parseSigns <*> parseUnsignedTeXInt

parseEightBitTeXInt :: InhibitableStream s => SimpParser s EightBitTeXInt
parseEightBitTeXInt = EightBitTeXInt <$> parseTeXInt

parseSigns :: InhibitableStream s => SimpParser s T.Sign
parseSigns = mconcat <$> parseOptionalSigns
  where
    parseOptionalSigns = skipOptionalSpaces
        *> P.sepEndBy (satisfyThen signToPos) skipOptionalSpaces

    signToPos t
        | matchOtherToken '+' t = Just $ T.Sign True
        | matchOtherToken '-' t = Just $ T.Sign False
        | otherwise = Nothing

parseUnsignedTeXInt :: InhibitableStream s => SimpParser s UnsignedTeXInt
parseUnsignedTeXInt = P.choice [ NormalTeXIntAsUTeXInt <$> parseNormalTeXInt
                               , CoercedTeXInt <$> parseCoercedTeXInt
                               ]

-- Restrict pure type, and therefore accumulator, to Int, to disallow
-- overflow.
digitsToTeXIntVal :: Integral n => n -> [n] -> TeXIntVal
digitsToTeXIntVal base =
    foldl' (\a b -> a * fromIntegral base + fromIntegral b) 0

parseNormalTeXInt :: InhibitableStream s => SimpParser s NormalTeXInt
parseNormalTeXInt =
    P.choice [ InternalTeXInt <$> parseInternalTeXInt
             , TeXIntConstant <$> parseConstantInt <* skipOneOptionalSpace
             ]
  where
    parseConstantInt = P.choice [ parseConstant, parseCharacter ]

    parseConstant = do
        (digits, base) <- P.choice [ (, 10) <$> P.some parseDecimalTeXIntDigit
                                   , (, 16) <$> parseHexadecimalTeXIntDigits
                                   , (, 8) <$> parseOctalTeXIntDigits
                                   ]
        pure $ fromIntegral $ digitsToTeXIntVal base digits

    parseCharacter = skipSatisfied (matchOtherToken '`') *> parseCharLike

parseDecimalTeXIntDigit :: InhibitableStream s => SimpParser s Int
parseDecimalTeXIntDigit = satisfyThen $
    \case
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other)) ->
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
        _ -> Nothing

parseHexadecimalTeXIntDigits :: InhibitableStream s => SimpParser s [Int]
parseHexadecimalTeXIntDigits = skipSatisfied (matchOtherToken '"')
    *> P.some (satisfyThen hexCharToInt)
  where
    hexCharToInt (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) = case c of
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

parseOctalTeXIntDigits :: InhibitableStream s => SimpParser s [Int]
parseOctalTeXIntDigits = skipSatisfied (matchOtherToken '\'')
    *> P.some (satisfyThen octCharToInt)
  where
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

parseCoercedTeXInt :: InhibitableStream s => SimpParser s CoercedTeXInt
parseCoercedTeXInt = P.choice [ InternalLengthAsInt <$> parseInternalLength
                               , InternalGlueAsInt <$> parseInternalGlue
                               ]

-- Length.
parseLength :: InhibitableStream s => SimpParser s Length
parseLength = Length <$> parseSigns <*> parseUnsignedLength

parseUnsignedLength :: InhibitableStream s => SimpParser s UnsignedLength
parseUnsignedLength = P.choice [ NormalLengthAsULength <$> parseNormalLength
                               , CoercedLength <$> parseCoercedLength
                               ]

parseNormalLength :: InhibitableStream s => SimpParser s NormalLength
parseNormalLength = P.choice [ LengthSemiConstant <$> parseFactor <*> parseUnit
                             , InternalLength <$> parseInternalLength
                             ]

parseFactor :: InhibitableStream s => SimpParser s Factor

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
parseFactor = P.choice $
    P.try <$> [ RationalConstant <$> parseRationalConstant
              , NormalTeXIntFactor <$> parseNormalTeXInt
              ]

parseRationalConstant :: InhibitableStream s => SimpParser s Rational
parseRationalConstant = do
    wholeNr <- decDigitsToTeXInt <$> P.many parseDecimalTeXIntDigit
    skipSatisfied (\t -> matchOtherToken ',' t || matchOtherToken '.' t)
    -- The fractional part represents its integer interpretation, divided by
    -- the next largest power of 10.
    -- TODO: If performance matters, maybe we can infer the denominator
    -- faster from the integer itself than the digits.
    fracDigits <- P.many parseDecimalTeXIntDigit
    let fraction = fromIntegral (decDigitsToTeXInt fracDigits)
            % (10 ^ length fracDigits)
    -- Convert the whole number to a rational, and add it to the fraction.
    pure $ fromIntegral wholeNr + fraction
  where
    decDigitsToTeXInt = digitsToTeXIntVal 10

parseUnit :: InhibitableStream s => SimpParser s Unit
parseUnit =
    P.choice [ P.try $
                   skipOptionalSpaces *> (InternalUnit <$> parseInternalUnit)
             , (PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit)
                   <* skipOneOptionalSpace
             ]
  where
    parseInternalUnit =
        P.choice [ parseInternalUnitLit <* skipOneOptionalSpace
                 , InternalTeXIntUnit <$> parseInternalTeXInt
                 , InternalLengthUnit <$> parseInternalLength
                 , InternalGlueUnit <$> parseInternalGlue
                 ]

    parseInternalUnitLit = P.choice $
        P.try <$> [ parseKeywordToValue "em" Em, parseKeywordToValue "ex" Ex ]

    parseFrame = do
        isTrue <- parseOptionalKeyword "true"
        pure $
            if isTrue
            then TrueFrame
            else MagnifiedFrame

    -- TODO: Use 'try' because keywords with common prefixes lead the parser
    -- down a blind alley. Could refactor to avoid, but it would be ugly.
    -- Leave as later optimisation.
    -- TODO: Should we omit the last try in such cases?
    -- NOTE: Can't trim number of 'try's naïvely, because they all suck up
    -- initial space, which would also need backtracking.
    parsePhysicalUnitLit = P.choice $
        P.try <$> [ parseKeywordToValue "bp" BigPoint
                  , parseKeywordToValue "cc" Cicero
                  , parseKeywordToValue "cm" Centimetre
                  , parseKeywordToValue "dd" Didot
                  , parseKeywordToValue "in" Inch
                  , parseKeywordToValue "mm" Millimetre
                  , parseKeywordToValue "pc" Pica
                  , parseKeywordToValue "pt" Point
                  , parseKeywordToValue "sp" ScaledPoint
                  ]

parseCoercedLength :: InhibitableStream s => SimpParser s CoercedLength
parseCoercedLength = InternalGlueAsLength <$> parseInternalGlue

-- Math length.
parseMathLength :: InhibitableStream s => SimpParser s MathLength
parseMathLength = MathLength <$> parseSigns <*> parseUnsignedMathLength

parseUnsignedMathLength :: InhibitableStream s
                        => SimpParser s UnsignedMathLength
parseUnsignedMathLength =
    P.choice [ NormalMathLengthAsUMathLength <$> parseNormalMathLength
             , CoercedMathLength <$> parseCoercedMathLength
             ]

parseNormalMathLength :: InhibitableStream s => SimpParser s NormalMathLength
parseNormalMathLength =
    MathLengthSemiConstant <$> parseFactor <*> parseMathUnit

parseMathUnit :: InhibitableStream s => SimpParser s MathUnit
parseMathUnit =
    P.choice [ skipKeyword "mu" >> skipOneOptionalSpace $> Mu
             , skipOptionalSpaces
                   *> (InternalMathGlueAsUnit <$> parseInternalMathGlue)
             ]

parseCoercedMathLength :: InhibitableStream s => SimpParser s CoercedMathLength
parseCoercedMathLength = InternalMathGlueAsMathLength <$> parseInternalMathGlue

-- Glue.
parseGlue :: InhibitableStream s => SimpParser s Glue
parseGlue = P.choice [ ExplicitGlue <$> parseLength
                           <*> parseFlex "plus"
                           <*> parseFlex "minus"
                     , InternalGlue <$> parseSigns <*> parseInternalGlue
                     ]

parseFlex :: InhibitableStream s => [CharCode] -> SimpParser s (Maybe Flex)
parseFlex s = P.choice [ Just <$> P.try parsePresentFlex
                       , skipOptionalSpaces $> Nothing
                       ]
  where
    parsePresentFlex =
        skipKeyword s
        *> P.choice (P.try <$> [FiniteFlex <$> parseLength, FilFlex <$> parseFilLength])

parseFilLength :: InhibitableStream s => SimpParser s FilLength
parseFilLength = (FilLength <$> parseSigns <*> parseFactor <*> parseOrder)
    <* skipOptionalSpaces
  where
    parseSomeLs = P.some $ skipSatisfied $ matchNonActiveCharacterUncased 'l'

    parseOrder = skipKeyword "fi" *> (length <$> parseSomeLs)

-- Math glue.
parseMathGlue :: InhibitableStream s => SimpParser s MathGlue
parseMathGlue =
    P.choice [ ExplicitMathGlue <$> parseMathLength
                   <*> parseMathFlex "plus"
                   <*> parseMathFlex "minus"
             , InternalMathGlue <$> parseSigns <*> parseInternalMathGlue
             ]

parseMathFlex :: InhibitableStream s
              => [CharCode]
              -> SimpParser s (Maybe MathFlex)
parseMathFlex s = P.choice [ Just <$> P.try parsePresentFlex
                           , skipOptionalSpaces $> Nothing
                           ]
  where
    parsePresentFlex =
        skipKeyword s
        *> P.choice (P.try <$> [ FiniteMathFlex <$> parseMathLength, FilMathFlex <$> parseFilLength])

-- Internal quantities.
parseQuantityVariable
    :: InhibitableStream s
    => (T.PrimitiveToken -> Maybe p) -- Try to extract a parameter from a token.
    -> T.RegisterType
    -> SimpParser s (QuantVariable p)
parseQuantityVariable getParam rTyp =
    P.choice [ ParamVar <$> satisfyThen getParam
             , RegisterVar <$> parseShortRegRef
             , RegisterVar <$> parseRegRef
             ]
  where
    parseShortRegRef = satisfyThen $
        \case
            T.IntRefTok (T.RegQuantity typ) n
                | typ == rTyp -> Just $ EightBitTeXInt $ constTeXInt n
            _ -> Nothing

    parseRegRef =
        skipSatisfied (== T.RegisterVariableTok rTyp) >> parseEightBitTeXInt

parseTeXIntVariable :: InhibitableStream s => SimpParser s TeXIntVariable
parseTeXIntVariable = parseQuantityVariable getParam T.RegInt
  where
    getParam (T.IntParamVarTok p) = Just p
    getParam _ = Nothing

parseLengthVariable :: InhibitableStream s => SimpParser s LengthVariable
parseLengthVariable = parseQuantityVariable getParam T.RegLen
  where
    getParam (T.LenParamVarTok p) = Just p
    getParam _ = Nothing

parseGlueVariable :: InhibitableStream s => SimpParser s GlueVariable
parseGlueVariable = parseQuantityVariable getParam T.RegGlue
  where
    getParam (T.GlueParamVarTok p) = Just p
    getParam _ = Nothing

parseMathGlueVariable :: InhibitableStream s => SimpParser s MathGlueVariable
parseMathGlueVariable = parseQuantityVariable getParam T.RegMathGlue
  where
    getParam (T.MathGlueParamVarTok p) = Just p
    getParam _ = Nothing

parseTokenListVariable :: InhibitableStream s => SimpParser s TokenListVariable
parseTokenListVariable = parseQuantityVariable getParam T.RegTokenList
  where
    getParam (T.TokenListParamVarTok p) = Just p
    getParam _ = Nothing

parseInternalTeXInt :: InhibitableStream s => SimpParser s InternalTeXInt
parseInternalTeXInt =
    P.choice [ InternalTeXIntVariable <$> parseTeXIntVariable
             , InternalSpecialTeXInt <$> parseSpecialTeXInt
             , InternalCodeTableRef <$> parseCodeTableRef
             , InternalCharToken <$> parseCharToken
             , InternalMathCharToken <$> parseMathCharToken
             , InternalFontCharRef <$> parseFontCharRef
             , skipSatisfiedEquals T.LastPenaltyTok $> LastPenalty
             , skipSatisfiedEquals T.ParagraphShapeTok $> ParShape
             , skipSatisfiedEquals T.InputLineNrTok $> InputLineNr
             , skipSatisfiedEquals T.BadnessTok $> Badness
             ]

parseCharToken :: InhibitableStream s => SimpParser s TeXIntVal
parseCharToken = satisfyThen $
    \case
        T.IntRefTok T.CharQuantity c -> Just c
        _ -> Nothing

parseMathCharToken :: InhibitableStream s => SimpParser s TeXIntVal
parseMathCharToken = satisfyThen $
    \case
        T.IntRefTok T.MathCharQuantity c -> Just c
        _ -> Nothing

parseSpecialTeXInt :: InhibitableStream s => SimpParser s T.SpecialTeXInt
parseSpecialTeXInt = satisfyThen $
    \case
        T.SpecialTeXIntTok p -> Just p
        _ -> Nothing

parseCodeTableRef :: InhibitableStream s => SimpParser s CodeTableRef
parseCodeTableRef = CodeTableRef <$> satisfyThen tokToCodeType <*> parseTeXInt
  where
    tokToCodeType (T.CodeTypeTok c) = Just c
    tokToCodeType _ = Nothing

parseFontCharRef :: InhibitableStream s => SimpParser s FontCharRef
parseFontCharRef = FontCharRef <$> satisfyThen tokToFontChar <*> parseFontRef
  where
    tokToFontChar (T.FontCharTok c) = Just c
    tokToFontChar _ = Nothing

parseFontRef :: InhibitableStream s => SimpParser s FontRef
parseFontRef = P.choice [ FontTokenRef <$> parseFontRefToken
                        , skipSatisfiedEquals T.FontTok $> CurrentFontRef
                        , FamilyMemberFontRef <$> parseFamilyMember
                        ]

parseFontRefToken :: InhibitableStream s => SimpParser s TeXIntVal
parseFontRefToken = satisfyThen $
    \case
        T.FontRefToken n -> Just n
        _ -> Nothing

parseFamilyMember :: InhibitableStream s => SimpParser s FamilyMember
parseFamilyMember = FamilyMember <$> satisfyThen tokToFontRange
    <*> parseTeXInt
  where
    tokToFontRange (T.FontRangeTok r) = Just r
    tokToFontRange _ = Nothing

parseInternalLength :: InhibitableStream s => SimpParser s InternalLength
parseInternalLength =
    P.choice [ InternalLengthVariable <$> parseLengthVariable
             , InternalSpecialLength <$> parseSpecialLength
             , InternalFontDimensionRef <$> parseFontDimensionRef
             , InternalBoxDimensionRef <$> parseBoxDimensionRef
             , skipSatisfiedEquals T.LastKernTok $> LastKern
             ]

parseSpecialLength :: InhibitableStream s => SimpParser s T.SpecialLength
parseSpecialLength = satisfyThen $
    \case
        T.SpecialLengthTok p -> Just p
        _ -> Nothing

parseFontDimensionRef :: InhibitableStream s => SimpParser s FontDimensionRef
parseFontDimensionRef = skipSatisfiedEquals T.FontDimensionTok
    >> (FontDimensionRef <$> parseTeXInt <*> parseFontRef)

parseBoxDimensionRef :: InhibitableStream s => SimpParser s BoxDimensionRef
parseBoxDimensionRef = do
    dim <- parseBoxDimension
    boxNr <- parseEightBitTeXInt
    pure $ BoxDimensionRef boxNr dim

parseBoxDimension :: InhibitableStream s => SimpParser s BoxDim
parseBoxDimension = satisfyThen $
    \case
        T.BoxDimensionTok d -> Just d
        _ -> Nothing

parseInternalGlue :: InhibitableStream s => SimpParser s InternalGlue
parseInternalGlue = P.choice [ InternalGlueVariable <$> parseGlueVariable
                             , skipSatisfiedEquals T.LastGlueTok $> LastGlue
                             ]

parseInternalMathGlue :: InhibitableStream s => SimpParser s InternalMathGlue
parseInternalMathGlue =
    P.choice [ InternalMathGlueVariable <$> parseMathGlueVariable
             , skipSatisfiedEquals T.LastGlueTok $> LastMathGlue
             ]

parseBox :: InhibitableStream s => SimpParser s Box
parseBox = P.choice [ parseRegisterBox
                    , skipSatisfiedEquals T.LastBoxTok $> LastBox
                    , parseVSplitBox
                    , parseExplicitBox
                    ]

parseRegisterBox :: InhibitableStream s => SimpParser s Box
parseRegisterBox = FetchedRegisterBox <$> parseFetchMode <*> parseEightBitTeXInt
  where
    parseFetchMode = satisfyThen $
        \case
            T.FetchedBoxTok m -> Just m
            _ -> Nothing

parseVSplitBox :: InhibitableStream s => SimpParser s Box
parseVSplitBox = do
    skipSatisfiedEquals T.SplitVBoxTok
    nr <- parseTeXInt
    skipKeyword "to"
    VSplitBox nr <$> parseLength

parseExplicitBox :: InhibitableStream s => SimpParser s Box
parseExplicitBox = do
    bt <- parseBoxType
    bs <- parseBoxSpecification
    skipLeftBrace
    pure $ ExplicitBox bs bt
  where
    parseBoxSpecification =
        P.choice [ skipKeyword "to" *> (To <$> parseLength)
                 , skipKeyword "spread" *> (Spread <$> parseLength)
                 , pure Natural
                 ]
        <* skipFiller

    parseBoxType = satisfyThen $
        \case
            T.ExplicitBoxTok b -> Just b
            _ -> Nothing
