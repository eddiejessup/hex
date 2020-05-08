{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Hex.Parse.Quantity where

import           Hexlude

import qualified Control.Monad.Combinators as PC
import qualified Data.Ascii                as Ascii

import           Hex.Config.Codes          (unsafeCodesFromChars)
import qualified Hex.Config.Codes          as Code
import qualified Hex.Lex                   as Lex
import           Hex.Parse.AST
import           Hex.Parse.Stream.Class
import qualified Hex.Resolve.Token           as T
import qualified Hex.Quantity              as Q

parseSigned :: TeXParseCtx st e m => m a -> m (T.Signed a)
parseSigned parseQuantity = T.Signed <$> parseSigns <*> parseQuantity

-- TeXInt.

parseTeXInt :: TeXParseCtx st e m => m TeXInt
parseTeXInt = parseSigned parseUnsignedTeXInt

parseEightBitTeXInt :: TeXParseCtx st e m => m EightBitTeXInt
parseEightBitTeXInt = EightBitTeXInt <$> parseTeXInt

parseSigns :: TeXParseCtx st e m => m T.Sign
parseSigns = mconcat <$> parseOptionalSigns
  where
    parseOptionalSigns = skipOptionalSpaces
        *> PC.sepEndBy (satisfyThen signToPos) skipOptionalSpaces

    signToPos t
        | matchOtherToken '+' t = Just T.Positive
        | matchOtherToken '-' t = Just T.Negative
        | otherwise = Nothing

parseUnsignedTeXInt :: TeXParseCtx st e m => m UnsignedTeXInt
parseUnsignedTeXInt =
    PC.choice
        [ NormalTeXIntAsUTeXInt <$> parseNormalTeXInt
        , CoercedTeXInt <$> parseHeaded headToParseCoercedTeXInt
        ]

digitsToTeXInt :: Int -> [Int] -> Q.TeXInt
digitsToTeXInt base digs =
    Q.TeXInt $ foldl' (\a b -> a * base + b) 0 digs

parseNormalTeXInt :: TeXParseCtx st e m => m NormalTeXInt
parseNormalTeXInt =
    PC.choice [ TeXIntConstant <$> parseConstantInt <* skipOneOptionalSpace
              , InternalTeXInt <$> parseHeaded headToParseInternalTeXInt
              ]
  where
    parseConstantInt =
        PC.choice
            [ digitsToTeXInt 10 <$> PC.some (satisfyThen decCharToInt)
            , digitsToTeXInt 16 <$> (skipSatisfied (matchOtherToken '"') *> PC.some (satisfyThen hexCharToInt))
            , digitsToTeXInt 8  <$> (skipSatisfied (matchOtherToken '\'') *> PC.some (satisfyThen octCharToInt))
            , Code.toTeXInt <$> (skipSatisfied (matchOtherToken '`') *> parseCharLike)
            ]

decCharToInt :: T.PrimitiveToken -> Maybe Int
decCharToInt = \case
    T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
        Ascii.fromDecDigit $ Code.codeWord c
    _ ->
        Nothing

hexCharToInt :: T.PrimitiveToken -> Maybe Int
hexCharToInt = \case
    T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
        Ascii.fromUpHexDigit $ Code.codeWord c
    T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c Code.Letter)) ->
        Ascii.fromUpHexAF $ Code.codeWord c
    _ -> Nothing

octCharToInt :: T.PrimitiveToken -> Maybe Int
octCharToInt = \case
    T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
        Ascii.fromOctDigit $ Code.codeWord c
    _ ->
        Nothing

headToParseCoercedTeXInt :: TeXParseCtx st e m => T.PrimitiveToken -> m CoercedTeXInt
headToParseCoercedTeXInt =
    choiceFlap
        [ fmap InternalLengthAsInt <$> headToParseInternalLength
        , fmap InternalGlueAsInt <$> headToParseInternalGlue
        ]

-- Length.
parseLength :: TeXParseCtx st e m => m Length
parseLength = parseSigned parseUnsignedLength

parseUnsignedLength :: TeXParseCtx st e m => m UnsignedLength
parseUnsignedLength = PC.choice [ NormalLengthAsULength <$> parseNormalLength
                                , CoercedLength . InternalGlueAsLength <$> parseHeaded headToParseInternalGlue
                                ]

parseNormalLength :: TeXParseCtx st e m => m NormalLength
parseNormalLength =
    PC.choice
        [ LengthSemiConstant <$> parseFactor <*> parseUnit
        , InternalLength <$> parseHeaded headToParseInternalLength
        ]

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
parseFactor :: TeXParseCtx st e m => m Factor
parseFactor = PC.choice
    [ RationalConstant <$> parseRationalConstant
    , NormalTeXIntFactor <$> parseNormalTeXInt
    ]

parseRationalConstant :: TeXParseCtx st e m => m Rational
parseRationalConstant = do
    wholeNr <- decDigitsToTeXInt <$> PC.many (satisfyThen decCharToInt)
    skipSatisfied (\t -> matchOtherToken ',' t || matchOtherToken '.' t)
    -- The fractional part represents its integer interpretation, divided by
    -- the next largest power of 10.
    -- TODO: If performance matters, maybe we can infer the denominator
    -- faster from the integer itself than the digits.
    fracDigits <- PC.many (satisfyThen decCharToInt)
    let fraction = fromIntegral (decDigitsToTeXInt fracDigits) `mkRatio` (10 ^ length fracDigits)
    -- Convert the whole number to a rational, and add it to the fraction.
    pure $ fromIntegral wholeNr + fraction
  where
    decDigitsToTeXInt = digitsToTeXInt 10

parseUnit :: TeXParseCtx st e m => m Unit
parseUnit =
    PC.choice
        [ skipOptionalSpaces *> (InternalUnit <$> parseInternalUnit)
        , (PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* skipOneOptionalSpace
        ]
  where
    parseInternalUnit =
        PC.choice
            [ parseInternalUnitLit <* skipOneOptionalSpace
            , InternalTeXIntUnit <$> parseHeaded headToParseInternalTeXInt
            , InternalLengthUnit <$> parseHeaded headToParseInternalLength
            , InternalGlueUnit <$> parseHeaded headToParseInternalGlue
            ]

    parseInternalUnitLit =
        PC.choice
            [ skipKeyword (unsafeCodesFromChars "em") $> Em
            , skipKeyword (unsafeCodesFromChars "ex") $> Ex
            ]

    parseFrame = do
        isTrue <- parseOptionalKeyword (unsafeCodesFromChars "true")
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
        [ skipKeyword (unsafeCodesFromChars "bp") $> Q.BigPoint
        , skipKeyword (unsafeCodesFromChars "cc") $> Q.Cicero
        , skipKeyword (unsafeCodesFromChars "cm") $> Q.Centimetre
        , skipKeyword (unsafeCodesFromChars "dd") $> Q.Didot
        , skipKeyword (unsafeCodesFromChars "in") $> Q.Inch
        , skipKeyword (unsafeCodesFromChars "mm") $> Q.Millimetre
        , skipKeyword (unsafeCodesFromChars "pc") $> Q.Pica
        , skipKeyword (unsafeCodesFromChars "pt") $> Q.Point
        , skipKeyword (unsafeCodesFromChars "sp") $> Q.ScaledPoint
        ]

-- Math length.
parseMathLength :: TeXParseCtx st e m => m MathLength
parseMathLength = parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: TeXParseCtx st e m => m UnsignedMathLength
parseUnsignedMathLength =
    PC.choice [ NormalMathLengthAsUMathLength <$> parseNormalMathLength
              , CoercedMathLength . InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
              ]

parseNormalMathLength :: TeXParseCtx st e m => m NormalMathLength
parseNormalMathLength =
    MathLengthSemiConstant <$> parseFactor <*> parseMathUnit

parseMathUnit :: TeXParseCtx st e m => m MathUnit
parseMathUnit =
    PC.choice [ skipKeyword (unsafeCodesFromChars "mu") >> skipOneOptionalSpace $> Mu
              , skipOptionalSpaces *> (InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
              ]

-- Glue.
parseGlue :: TeXParseCtx st e m => m Glue
parseGlue = PC.choice [ ExplicitGlue <$> parseLength <*> parseFlex (unsafeCodesFromChars "plus") <*> parseFlex (unsafeCodesFromChars "minus")
                      , InternalGlue <$> parseSigned (parseHeaded headToParseInternalGlue)
                      ]

parseFlex :: TeXParseCtx st e m => [Code.CharCode] -> m (Maybe Flex)
parseFlex s =
    PC.choice
        [ Just <$> parsePresentFlex
        , skipOptionalSpaces $> Nothing
        ]
  where
    parsePresentFlex =
        do
        skipKeyword s
        PC.choice
            [ FiniteFlex <$> parseLength
            , FilFlex <$> parseFilLength
            ]

parseFilLength :: TeXParseCtx st e m => m FilLength
parseFilLength =
    (FilLength <$> parseSigned parseFactor <*> parseOrder) <* skipOptionalSpaces
  where
    parseSomeLs = PC.some $ skipSatisfied $ matchNonActiveCharacterUncased (Code.unsafeCodeFromChar 'l')

    parseOrder = skipKeyword (unsafeCodesFromChars "fi") *> (length <$> parseSomeLs)

-- Math glue.
parseMathGlue :: TeXParseCtx st e m => m MathGlue
parseMathGlue =
    PC.choice [ ExplicitMathGlue <$> parseMathLength <*> parseMathFlex (unsafeCodesFromChars "plus") <*> parseMathFlex (unsafeCodesFromChars "minus")
              , InternalMathGlue <$> parseSigns <*> parseHeaded headToParseInternalMathGlue
              ]

parseMathFlex :: TeXParseCtx st e m => [Code.CharCode] -> m (Maybe MathFlex)
parseMathFlex s = PC.choice [ Just <$> parsePresentFlex
                            , skipOptionalSpaces $> Nothing
                            ]
  where
    parsePresentFlex =
        skipKeyword s
        *> PC.choice [ FiniteMathFlex <$> parseMathLength
                     , FilMathFlex <$> parseFilLength]

-- Internal quantities.

headToParseTeXIntVariable :: TeXParseCtx st e m => T.PrimitiveToken -> m TeXIntVariable
headToParseTeXIntVariable = \case
    T.IntParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegInt) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegInt ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseLengthVariable :: TeXParseCtx st e m => T.PrimitiveToken -> m LengthVariable
headToParseLengthVariable = \case
    T.LenParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegLen) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegLen ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseGlueVariable :: TeXParseCtx st e m => T.PrimitiveToken -> m GlueVariable
headToParseGlueVariable = \case
    T.GlueParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegGlue) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegGlue ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseMathGlueVariable :: TeXParseCtx st e m => T.PrimitiveToken -> m MathGlueVariable
headToParseMathGlueVariable = \case
    T.MathGlueParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegMathGlue) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegMathGlue ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseTokenListVariable :: TeXParseCtx st e m => T.PrimitiveToken -> m TokenListVariable
headToParseTokenListVariable = \case
    T.TokenListParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegTokenList) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegTokenList ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseSpecialTeXInt :: TeXParseCtx st e m => T.PrimitiveToken -> m T.SpecialTeXInt
headToParseSpecialTeXInt = \case
    T.SpecialTeXIntTok p ->
        pure p
    _ ->
        empty

headToParseCodeTableRef :: TeXParseCtx st e m => T.PrimitiveToken -> m CodeTableRef
headToParseCodeTableRef = \case
    T.CodeTypeTok c ->
        CodeTableRef c <$> parseTeXInt
    t ->
        parseError $ ParseError $ "Expected 'CodeTypeTok', saw " <> show t

headToParseCharToken :: TeXParseCtx st e m => T.PrimitiveToken -> m Q.TeXInt
headToParseCharToken = \case
    T.IntRefTok T.CharQuantity c ->
        pure c
    t ->
        parseError $ ParseError $ "Expected 'IntRefTok CharQuantity', saw " <> show t

headToParseMathCharToken :: TeXParseCtx st e m => T.PrimitiveToken -> m Q.TeXInt
headToParseMathCharToken = \case
    T.IntRefTok T.MathCharQuantity c ->
        pure c
    _ ->
        empty

headToParseFontCharRef :: TeXParseCtx st e m => T.PrimitiveToken -> m FontCharRef
headToParseFontCharRef = \case
    T.FontCharTok c ->
        FontCharRef c <$> parseHeaded headToParseFontRef
    _ ->
        empty

headToParseInternalTeXInt :: TeXParseCtx st e m => T.PrimitiveToken -> m InternalTeXInt
headToParseInternalTeXInt =
    choiceFlap
        [ \case
            T.LastPenaltyTok    -> pure LastPenalty
            T.ParagraphShapeTok -> pure ParShape
            T.InputLineNrTok    -> pure InputLineNr
            T.BadnessTok        -> pure Badness
            _                   -> empty
        , fmap InternalTeXIntVariable <$> headToParseTeXIntVariable
        , fmap InternalSpecialTeXInt <$> headToParseSpecialTeXInt
        , fmap InternalCodeTableRef <$> headToParseCodeTableRef
        , fmap InternalCharToken <$> headToParseCharToken
        , fmap InternalMathCharToken <$> headToParseMathCharToken
        , fmap InternalFontCharRef <$> headToParseFontCharRef
        ]

headToParseFontRef :: TeXParseCtx st e m => T.PrimitiveToken -> m FontRef
headToParseFontRef =
    choiceFlap
        [ \case
                T.FontTok -> pure CurrentFontRef
                _ -> empty
        , fmap FontTokenRef <$> headToParseFontRefToken
        , fmap FamilyMemberFontRef <$> headToParseFamilyMember
        ]

headToParseFontRefToken :: TeXParseCtx st e m => T.PrimitiveToken -> m Q.TeXInt
headToParseFontRefToken = \case
    T.FontRefToken n -> pure n
    _                -> empty

headToParseFamilyMember :: TeXParseCtx st e m => T.PrimitiveToken -> m FamilyMember
headToParseFamilyMember = \case
    T.FontRangeTok r -> FamilyMember r <$> parseTeXInt
    _                -> empty

headToParseInternalLength :: TeXParseCtx st e m => T.PrimitiveToken -> m InternalLength
headToParseInternalLength =
    choiceFlap
        [ \case
                T.LastKernTok -> pure LastKern
                _ -> empty
        , fmap InternalLengthVariable <$> headToParseLengthVariable
        , fmap InternalSpecialLength <$> headToParseSpecialLength
        , fmap InternalFontDimensionRef <$> headToParseFontDimensionRef
        , fmap InternalBoxDimensionRef <$> headToParseBoxDimensionRef
        ]

headToParseSpecialLength :: TeXParseCtx st e m => T.PrimitiveToken -> m T.SpecialLength
headToParseSpecialLength = \case
    T.SpecialLengthTok p ->
        pure p
    _ ->
        empty

headToParseFontDimensionRef :: TeXParseCtx st e m => T.PrimitiveToken -> m FontDimensionRef
headToParseFontDimensionRef = \case
    T.FontDimensionTok ->
        FontDimensionRef <$> parseTeXInt <*> parseHeaded headToParseFontRef
    _ ->
        empty

headToParseBoxDimensionRef :: TeXParseCtx st e m => T.PrimitiveToken -> m BoxDimensionRef
headToParseBoxDimensionRef = \case
    T.BoxDimensionTok dim ->
        do
        boxNr <- parseEightBitTeXInt
        pure $ BoxDimensionRef boxNr dim
    _ ->
        empty

headToParseInternalGlue :: TeXParseCtx st e m => T.PrimitiveToken -> m InternalGlue
headToParseInternalGlue =
    choiceFlap
        [ fmap InternalGlueVariable <$> headToParseGlueVariable
        , \case
            T.LastGlueTok -> pure LastGlue
            _             -> empty
        ]

headToParseInternalMathGlue :: TeXParseCtx st e m => T.PrimitiveToken -> m InternalMathGlue
headToParseInternalMathGlue =
    choiceFlap
        [ fmap InternalMathGlueVariable <$> headToParseMathGlueVariable
        , \case
            T.LastGlueTok ->
                pure LastMathGlue
            _ ->
                empty
        ]

headToParseBox :: TeXParseCtx st e m => T.PrimitiveToken -> m Box
headToParseBox = \case
    T.FetchedBoxTok fetchMode ->
        FetchedRegisterBox fetchMode <$> parseEightBitTeXInt
    T.LastBoxTok ->
        pure LastBox
    T.SplitVBoxTok ->
        do
        nr <- parseTeXInt
        skipKeyword (unsafeCodesFromChars "to")
        VSplitBox nr <$> parseLength
    T.ExplicitBoxTok boxType ->
        do
        boxSpec <- parseBoxSpecification
        skipLeftBrace
        pure $ ExplicitBox boxSpec boxType
    _ ->
        empty
  where
    parseBoxSpecification =
        PC.choice
            [ skipKeyword (unsafeCodesFromChars "to") *> (To <$> parseLength)
            , skipKeyword (unsafeCodesFromChars "spread") *> (Spread <$> parseLength)
            , pure Natural
            ] <* skipFiller
