{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Quantity where

import           HeXlude

import qualified Control.Monad.Combinators as PC
import           Data.Foldable       ( foldl' )
import           Data.Functor        ( ($>) )
import           Data.Ratio          ( (%) )

import           HeX.Categorise      ( CharCode )
import qualified HeX.Lex             as Lex
import           HeX.Parse.AST
import           HeX.Parse.Stream.Class
import qualified HeX.Parse.Token     as T
import           HeX.Unit            ( PhysicalUnit(..) )

-- TeXInt.

parseTeXInt :: TeXParser s TeXInt
parseTeXInt = TeXInt <$> parseSigns <*> parseUnsignedTeXInt

parseEightBitTeXInt :: TeXParser s EightBitTeXInt
parseEightBitTeXInt = EightBitTeXInt <$> parseTeXInt

parseSigns :: TeXParser s T.Sign
parseSigns = mconcat <$> parseOptionalSigns
  where
    parseOptionalSigns = skipOptionalSpaces
        *> PC.sepEndBy (satisfyThen signToPos) skipOptionalSpaces

    signToPos t
        | matchOtherToken '+' t = Just $ T.Sign True
        | matchOtherToken '-' t = Just $ T.Sign False
        | otherwise = Nothing

parseUnsignedTeXInt :: TeXParser s UnsignedTeXInt
parseUnsignedTeXInt = PC.choice [ NormalTeXIntAsUTeXInt <$> parseNormalTeXInt
                                , CoercedTeXInt <$> parseCoercedTeXInt
                                ]

-- Restrict pure type, and therefore accumulator, to Int, to disallow
-- overflow.
digitsToTeXIntVal :: Integral n => n -> [n] -> TeXIntVal
digitsToTeXIntVal base =
    foldl' (\a b -> a * fromIntegral base + fromIntegral b) 0

parseNormalTeXInt :: TeXParser s NormalTeXInt
parseNormalTeXInt =
    PC.choice [ InternalTeXInt <$> parseInternalTeXInt
              , TeXIntConstant <$> parseConstantInt <* skipOneOptionalSpace
              ]
  where
    parseConstantInt = PC.choice [ parseConstant, parseCharacter ]

    parseConstant = do
        (digits, base) <- PC.choice [ (, 10) <$> PC.some parseDecimalTeXIntDigit
                                   , (, 16) <$> parseHexadecimalTeXIntDigits
                                   , (, 8) <$> parseOctalTeXIntDigits
                                   ]
        pure $ fromIntegral $ digitsToTeXIntVal base digits

    parseCharacter = skipSatisfied (matchOtherToken '`') *> parseCharLike

parseDecimalTeXIntDigit :: TeXParser s Int
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

parseHexadecimalTeXIntDigits :: TeXParser s [Int]
parseHexadecimalTeXIntDigits = skipSatisfied (matchOtherToken '"')
    *> PC.some (satisfyThen hexCharToInt)
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

parseOctalTeXIntDigits :: TeXParser s [Int]
parseOctalTeXIntDigits = skipSatisfied (matchOtherToken '\'')
    *> PC.some (satisfyThen octCharToInt)
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

parseCoercedTeXInt :: TeXParser s CoercedTeXInt
parseCoercedTeXInt = PC.choice [ InternalLengthAsInt <$> parseInternalLength
                               , InternalGlueAsInt <$> parseInternalGlue
                               ]

-- Length.
parseLength :: TeXParser s Length
parseLength = Length <$> parseSigns <*> parseUnsignedLength

parseUnsignedLength :: TeXParser s UnsignedLength
parseUnsignedLength = PC.choice [ NormalLengthAsULength <$> parseNormalLength
                                , CoercedLength <$> parseCoercedLength
                                ]

parseNormalLength :: TeXParser s NormalLength
parseNormalLength = PC.choice [ LengthSemiConstant <$> parseFactor <*> parseUnit
                              , InternalLength <$> parseInternalLength
                              ]

parseFactor :: TeXParser s Factor

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
parseFactor = PC.choice
    [ RationalConstant <$> parseRationalConstant
    , NormalTeXIntFactor <$> parseNormalTeXInt
    ]

parseRationalConstant :: TeXParser s Rational
parseRationalConstant = do
    wholeNr <- decDigitsToTeXInt <$> PC.many parseDecimalTeXIntDigit
    skipSatisfied (\t -> matchOtherToken ',' t || matchOtherToken '.' t)
    -- The fractional part represents its integer interpretation, divided by
    -- the next largest power of 10.
    -- TODO: If performance matters, maybe we can infer the denominator
    -- faster from the integer itself than the digits.
    fracDigits <- PC.many parseDecimalTeXIntDigit
    let fraction = fromIntegral (decDigitsToTeXInt fracDigits)
            % (10 ^ length fracDigits)
    -- Convert the whole number to a rational, and add it to the fraction.
    pure $ fromIntegral wholeNr + fraction
  where
    decDigitsToTeXInt = digitsToTeXIntVal 10

parseUnit :: TeXParser s Unit
parseUnit =
    PC.choice [ skipOptionalSpaces *> (InternalUnit <$> parseInternalUnit)
             , (PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit)
                   <* skipOneOptionalSpace
             ]
  where
    parseInternalUnit =
        PC.choice [ parseInternalUnitLit <* skipOneOptionalSpace
                  , InternalTeXIntUnit <$> parseInternalTeXInt
                  , InternalLengthUnit <$> parseInternalLength
                  , InternalGlueUnit <$> parseInternalGlue
                  ]

    parseInternalUnitLit =
        PC.choice [ parseKeywordToValue "em" Em, parseKeywordToValue "ex" Ex ]

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
    parsePhysicalUnitLit = PC.choice
        [ parseKeywordToValue "bp" BigPoint
        , parseKeywordToValue "cc" Cicero
        , parseKeywordToValue "cm" Centimetre
        , parseKeywordToValue "dd" Didot
        , parseKeywordToValue "in" Inch
        , parseKeywordToValue "mm" Millimetre
        , parseKeywordToValue "pc" Pica
        , parseKeywordToValue "pt" Point
        , parseKeywordToValue "sp" ScaledPoint
        ]

parseCoercedLength :: TeXParser s CoercedLength
parseCoercedLength = InternalGlueAsLength <$> parseInternalGlue

-- Math length.
parseMathLength :: TeXParser s MathLength
parseMathLength = MathLength <$> parseSigns <*> parseUnsignedMathLength

parseUnsignedMathLength :: TeXParser s UnsignedMathLength
parseUnsignedMathLength =
    PC.choice [ NormalMathLengthAsUMathLength <$> parseNormalMathLength
              , CoercedMathLength <$> parseCoercedMathLength
              ]

parseNormalMathLength :: TeXParser s NormalMathLength
parseNormalMathLength =
    MathLengthSemiConstant <$> parseFactor <*> parseMathUnit

parseMathUnit :: TeXParser s MathUnit
parseMathUnit =
    PC.choice [ skipKeyword "mu" >> skipOneOptionalSpace $> Mu
              , skipOptionalSpaces *> (InternalMathGlueAsUnit <$> parseInternalMathGlue)
              ]

parseCoercedMathLength :: TeXParser s CoercedMathLength
parseCoercedMathLength = InternalMathGlueAsMathLength <$> parseInternalMathGlue

-- Glue.
parseGlue :: TeXParser s Glue
parseGlue = PC.choice [ ExplicitGlue <$> parseLength <*> parseFlex "plus" <*> parseFlex "minus"
                      , InternalGlue <$> parseSigns <*> parseInternalGlue
                      ]

parseFlex :: [CharCode] -> TeXParser s (Maybe Flex)
parseFlex s = PC.choice [ Just <$> parsePresentFlex
                        , skipOptionalSpaces $> Nothing
                        ]
  where
    parsePresentFlex =
        skipKeyword s
        *> PC.choice [FiniteFlex <$> parseLength, FilFlex <$> parseFilLength]

parseFilLength :: TeXParser s FilLength
parseFilLength = (FilLength <$> parseSigns <*> parseFactor <*> parseOrder)
    <* skipOptionalSpaces
  where
    parseSomeLs = PC.some $ skipSatisfied $ matchNonActiveCharacterUncased 'l'

    parseOrder = skipKeyword "fi" *> (length <$> parseSomeLs)

-- Math glue.
parseMathGlue :: TeXParser s MathGlue
parseMathGlue =
    PC.choice [ ExplicitMathGlue <$> parseMathLength <*> parseMathFlex "plus" <*> parseMathFlex "minus"
              , InternalMathGlue <$> parseSigns <*> parseInternalMathGlue
              ]

parseMathFlex :: [CharCode] -> TeXParser s (Maybe MathFlex)
parseMathFlex s = PC.choice [ Just <$> parsePresentFlex
                            , skipOptionalSpaces $> Nothing
                            ]
  where
    parsePresentFlex =
        skipKeyword s
        *> PC.choice [ FiniteMathFlex <$> parseMathLength
                     , FilMathFlex <$> parseFilLength]

-- Internal quantities.
parseQuantityVariable
    :: (T.PrimitiveToken -> Maybe p) -- Try to extract a parameter from a token.
    -> T.RegisterType
    -> TeXParser s (QuantVariable p)
parseQuantityVariable getParam rTyp =
    PC.choice [ ParamVar <$> satisfyThen getParam
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

parseTeXIntVariable :: TeXParser s TeXIntVariable
parseTeXIntVariable = parseQuantityVariable getParam T.RegInt
  where
    getParam (T.IntParamVarTok p) = Just p
    getParam _ = Nothing

parseLengthVariable :: TeXParser s LengthVariable
parseLengthVariable = parseQuantityVariable getParam T.RegLen
  where
    getParam (T.LenParamVarTok p) = Just p
    getParam _ = Nothing

parseGlueVariable :: TeXParser s GlueVariable
parseGlueVariable = parseQuantityVariable getParam T.RegGlue
  where
    getParam (T.GlueParamVarTok p) = Just p
    getParam _ = Nothing

parseMathGlueVariable :: TeXParser s MathGlueVariable
parseMathGlueVariable = parseQuantityVariable getParam T.RegMathGlue
  where
    getParam (T.MathGlueParamVarTok p) = Just p
    getParam _ = Nothing

parseTokenListVariable :: TeXParser s TokenListVariable
parseTokenListVariable = parseQuantityVariable getParam T.RegTokenList
  where
    getParam (T.TokenListParamVarTok p) = Just p
    getParam _ = Nothing

parseInternalTeXInt :: TeXParser s InternalTeXInt
parseInternalTeXInt =
    PC.choice [ InternalTeXIntVariable <$> parseTeXIntVariable
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

parseCharToken :: TeXParser s TeXIntVal
parseCharToken = satisfyThen $
    \case
        T.IntRefTok T.CharQuantity c -> Just c
        _ -> Nothing

parseMathCharToken :: TeXParser s TeXIntVal
parseMathCharToken = satisfyThen $
    \case
        T.IntRefTok T.MathCharQuantity c -> Just c
        _ -> Nothing

parseSpecialTeXInt :: TeXParser s T.SpecialTeXInt
parseSpecialTeXInt = satisfyThen $
    \case
        T.SpecialTeXIntTok p -> Just p
        _ -> Nothing

parseCodeTableRef :: TeXParser s CodeTableRef
parseCodeTableRef = CodeTableRef <$> satisfyThen tokToCodeType <*> parseTeXInt
  where
    tokToCodeType (T.CodeTypeTok c) = Just c
    tokToCodeType _ = Nothing

parseFontCharRef :: TeXParser s FontCharRef
parseFontCharRef = FontCharRef <$> satisfyThen tokToFontChar <*> parseFontRef
  where
    tokToFontChar (T.FontCharTok c) = Just c
    tokToFontChar _ = Nothing

parseFontRef :: TeXParser s FontRef
parseFontRef = PC.choice [ FontTokenRef <$> parseFontRefToken
                         , skipSatisfiedEquals T.FontTok $> CurrentFontRef
                         , FamilyMemberFontRef <$> parseFamilyMember
                         ]

parseFontRefToken :: TeXParser s TeXIntVal
parseFontRefToken = satisfyThen $
    \case
        T.FontRefToken n -> Just n
        _ -> Nothing

parseFamilyMember :: TeXParser s FamilyMember
parseFamilyMember = FamilyMember <$> satisfyThen tokToFontRange
    <*> parseTeXInt
  where
    tokToFontRange (T.FontRangeTok r) = Just r
    tokToFontRange _ = Nothing

parseInternalLength :: TeXParser s InternalLength
parseInternalLength =
    PC.choice [ InternalLengthVariable <$> parseLengthVariable
              , InternalSpecialLength <$> parseSpecialLength
              , InternalFontDimensionRef <$> parseFontDimensionRef
              , InternalBoxDimensionRef <$> parseBoxDimensionRef
              , skipSatisfiedEquals T.LastKernTok $> LastKern
              ]

parseSpecialLength :: TeXParser s T.SpecialLength
parseSpecialLength = satisfyThen $
    \case
        T.SpecialLengthTok p -> Just p
        _ -> Nothing

parseFontDimensionRef :: TeXParser s FontDimensionRef
parseFontDimensionRef = skipSatisfiedEquals T.FontDimensionTok
    >> (FontDimensionRef <$> parseTeXInt <*> parseFontRef)

parseBoxDimensionRef :: TeXParser s BoxDimensionRef
parseBoxDimensionRef = do
    dim <- parseBoxDimension
    boxNr <- parseEightBitTeXInt
    pure $ BoxDimensionRef boxNr dim

parseBoxDimension :: TeXParser s BoxDim
parseBoxDimension = satisfyThen $
    \case
        T.BoxDimensionTok d -> Just d
        _ -> Nothing

parseInternalGlue :: TeXParser s InternalGlue
parseInternalGlue = PC.choice [ InternalGlueVariable <$> parseGlueVariable
                              , skipSatisfiedEquals T.LastGlueTok $> LastGlue
                              ]

parseInternalMathGlue :: TeXParser s InternalMathGlue
parseInternalMathGlue =
    PC.choice [ InternalMathGlueVariable <$> parseMathGlueVariable
              , skipSatisfiedEquals T.LastGlueTok $> LastMathGlue
              ]

parseBox :: TeXParser s Box
parseBox = PC.choice [ parseRegisterBox
                     , skipSatisfiedEquals T.LastBoxTok $> LastBox
                     , parseVSplitBox
                     , parseExplicitBox
                     ]

parseRegisterBox :: TeXParser s Box
parseRegisterBox = FetchedRegisterBox <$> parseFetchMode <*> parseEightBitTeXInt
  where
    parseFetchMode = satisfyThen $
        \case
            T.FetchedBoxTok m -> Just m
            _ -> Nothing

parseVSplitBox :: TeXParser s Box
parseVSplitBox = do
    skipSatisfiedEquals T.SplitVBoxTok
    nr <- parseTeXInt
    skipKeyword "to"
    VSplitBox nr <$> parseLength

parseExplicitBox :: TeXParser s Box
parseExplicitBox = do
    bt <- parseBoxType
    bs <- parseBoxSpecification
    skipLeftBrace
    pure $ ExplicitBox bs bt
  where
    parseBoxSpecification =
        PC.choice [ skipKeyword "to" *> (To <$> parseLength)
                  , skipKeyword "spread" *> (Spread <$> parseLength)
                  , pure Natural
                  ]
        <* skipFiller

    parseBoxType = satisfyThen $
        \case
            T.ExplicitBoxTok b -> Just b
            _ -> Nothing
