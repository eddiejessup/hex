{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Hex.Parse.Quantity where

import           Hexlude

import qualified Control.Monad.Combinators as PC
import qualified Data.Ascii                as Ascii
import qualified Text.Megaparsec           as P

import           Hex.Config.Codes          (unsafeCodesFromChars)
import qualified Hex.Config.Codes          as Code
import qualified Hex.Lex                   as Lex
import           Hex.Parse.AST
import           Hex.Parse.Stream.Class
import qualified Hex.Resolve.Token           as T
import qualified Hex.Quantity              as Q

parseSigned :: TeXParseable s e m => SimpleParsecT s m a -> SimpleParsecT s m (T.Signed a)
parseSigned parseQuantity = T.Signed <$> parseSigns <*> parseQuantity

-- TeXInt.

parseTeXInt :: TeXParser s e m TeXInt
parseTeXInt = parseSigned parseUnsignedTeXInt

parseEightBitTeXInt :: TeXParser s e m EightBitTeXInt
parseEightBitTeXInt = EightBitTeXInt <$> parseTeXInt

parseSigns :: TeXParser s e m T.Sign
parseSigns = mconcat <$> parseOptionalSigns
  where
    parseOptionalSigns = skipOptionalSpaces
        *> PC.sepEndBy (satisfyThen signToPos) skipOptionalSpaces

    signToPos t
        | matchOtherToken '+' t = Just T.Positive
        | matchOtherToken '-' t = Just T.Negative
        | otherwise = Nothing

parseUnsignedTeXInt :: TeXParser s e m UnsignedTeXInt
parseUnsignedTeXInt =
    tryChoice
        [ NormalTeXIntAsUTeXInt <$> parseNormalTeXInt
        , CoercedTeXInt <$> parseHeaded headToParseCoercedTeXInt
        ]

digitsToTeXInt :: Int -> [Int] -> Q.TeXInt
digitsToTeXInt base digs =
    Q.TeXInt $ foldl' (\a b -> a * base + b) 0 digs

parseNormalTeXInt :: TeXParser s e m NormalTeXInt
parseNormalTeXInt =
    tryChoice [ TeXIntConstant <$> parseConstantInt <* skipOneOptionalSpace
              , InternalTeXInt <$> parseHeaded headToParseInternalTeXInt
              ]
  where
    parseConstantInt =
        tryChoice
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

headToParseCoercedTeXInt :: T.PrimitiveToken -> TeXParser s e m CoercedTeXInt
headToParseCoercedTeXInt =
    choiceFlap
        [ fmap InternalLengthAsInt <$> headToParseInternalLength
        , fmap InternalGlueAsInt <$> headToParseInternalGlue
        ]

-- Length.
parseLength :: TeXParser s e m Length
parseLength = parseSigned parseUnsignedLength

parseUnsignedLength :: TeXParser s e m UnsignedLength
parseUnsignedLength = tryChoice [ NormalLengthAsULength <$> parseNormalLength
                                , CoercedLength . InternalGlueAsLength <$> parseHeaded headToParseInternalGlue
                                ]

parseNormalLength :: TeXParser s e m NormalLength
parseNormalLength =
    tryChoice
        [ LengthSemiConstant <$> parseFactor <*> parseUnit
        , InternalLength <$> parseHeaded headToParseInternalLength
        ]

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
parseFactor :: TeXParser s e m Factor
parseFactor = tryChoice
    [ RationalConstant <$> parseRationalConstant
    , NormalTeXIntFactor <$> parseNormalTeXInt
    ]

parseRationalConstant :: TeXParser s e m Rational
parseRationalConstant = do
    wholeNr <- decDigitsToTeXInt <$> PC.many (satisfyThen decCharToInt)
    skipSatisfied (\t -> matchOtherToken ',' t || matchOtherToken '.' t)
    -- The fractional part represents its integer interpretation, divided by
    -- the next largest power of 10.
    -- TODO: If performance matters, maybe we can infer the denominator
    -- faster from the integer itself than the digits.
    fracDigits <- PC.many (satisfyThen decCharToInt)
    let fraction = fromIntegral (decDigitsToTeXInt fracDigits) % (10 ^ length fracDigits)
    -- Convert the whole number to a rational, and add it to the fraction.
    pure $ fromIntegral wholeNr + fraction
  where
    decDigitsToTeXInt = digitsToTeXInt 10

parseUnit :: TeXParser s e m Unit
parseUnit =
    tryChoice
        [ skipOptionalSpaces *> (InternalUnit <$> parseInternalUnit)
        , (PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* skipOneOptionalSpace
        ]
  where
    parseInternalUnit =
        tryChoice
            [ parseInternalUnitLit <* skipOneOptionalSpace
            , InternalTeXIntUnit <$> parseHeaded headToParseInternalTeXInt
            , InternalLengthUnit <$> parseHeaded headToParseInternalLength
            , InternalGlueUnit <$> parseHeaded headToParseInternalGlue
            ]

    parseInternalUnitLit =
        tryChoice
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
    parsePhysicalUnitLit = tryChoice
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
parseMathLength :: TeXParser s e m MathLength
parseMathLength = parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: TeXParser s e m UnsignedMathLength
parseUnsignedMathLength =
    tryChoice [ NormalMathLengthAsUMathLength <$> parseNormalMathLength
              , CoercedMathLength . InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
              ]

parseNormalMathLength :: TeXParser s e m NormalMathLength
parseNormalMathLength =
    MathLengthSemiConstant <$> parseFactor <*> parseMathUnit

parseMathUnit :: TeXParser s e m MathUnit
parseMathUnit =
    tryChoice [ skipKeyword (unsafeCodesFromChars "mu") >> skipOneOptionalSpace $> Mu
              , skipOptionalSpaces *> (InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
              ]

-- Glue.
parseGlue :: TeXParser s e m Glue
parseGlue = tryChoice [ ExplicitGlue <$> parseLength <*> parseFlex (unsafeCodesFromChars "plus") <*> parseFlex (unsafeCodesFromChars "minus")
                      , InternalGlue <$> parseSigned (parseHeaded headToParseInternalGlue)
                      ]

parseFlex :: [Code.CharCode] -> TeXParser s e m (Maybe Flex)
parseFlex s =
    tryChoice
        [ Just <$> parsePresentFlex
        , skipOptionalSpaces $> Nothing
        ]
  where
    parsePresentFlex =
        do
        skipKeyword s
        tryChoice
            [ FiniteFlex <$> parseLength
            , FilFlex <$> parseFilLength
            ]

parseFilLength :: TeXParser s e m FilLength
parseFilLength =
    (FilLength <$> parseSigned parseFactor <*> parseOrder) <* skipOptionalSpaces
  where
    parseSomeLs = PC.some $ skipSatisfied $ matchNonActiveCharacterUncased (Code.unsafeCodeFromChar 'l')

    parseOrder = skipKeyword (unsafeCodesFromChars "fi") *> (length <$> parseSomeLs)

-- Math glue.
parseMathGlue :: TeXParser s e m MathGlue
parseMathGlue =
    tryChoice [ ExplicitMathGlue <$> parseMathLength <*> parseMathFlex (unsafeCodesFromChars "plus") <*> parseMathFlex (unsafeCodesFromChars "minus")
              , InternalMathGlue <$> parseSigns <*> parseHeaded headToParseInternalMathGlue
              ]

parseMathFlex :: [Code.CharCode] -> TeXParser s e m (Maybe MathFlex)
parseMathFlex s = tryChoice [ Just <$> parsePresentFlex
                            , skipOptionalSpaces $> Nothing
                            ]
  where
    parsePresentFlex =
        skipKeyword s
        *> tryChoice [ FiniteMathFlex <$> parseMathLength
                     , FilMathFlex <$> parseFilLength]

-- Internal quantities.

headToParseTeXIntVariable :: T.PrimitiveToken -> TeXParser s e m TeXIntVariable
headToParseTeXIntVariable = \case
    T.IntParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegInt) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegInt ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseLengthVariable :: T.PrimitiveToken -> TeXParser s e m LengthVariable
headToParseLengthVariable = \case
    T.LenParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegLen) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegLen ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseGlueVariable :: T.PrimitiveToken -> TeXParser s e m GlueVariable
headToParseGlueVariable = \case
    T.GlueParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegGlue) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegGlue ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseMathGlueVariable :: T.PrimitiveToken -> TeXParser s e m MathGlueVariable
headToParseMathGlueVariable = \case
    T.MathGlueParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegMathGlue) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegMathGlue ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseTokenListVariable :: T.PrimitiveToken -> TeXParser s e m TokenListVariable
headToParseTokenListVariable = \case
    T.TokenListParamVarTok p ->
        pure (ParamVar p)
    T.IntRefTok (T.RegQuantity T.RegTokenList) n ->
        pure $ RegisterVar $ EightBitTeXInt $ constTeXInt n
    T.RegisterVariableTok T.RegTokenList ->
        RegisterVar <$> parseEightBitTeXInt
    _ ->
        empty

headToParseSpecialTeXInt :: T.PrimitiveToken -> TeXParser s e m T.SpecialTeXInt
headToParseSpecialTeXInt = \case
    T.SpecialTeXIntTok p ->
        pure p
    _ ->
        empty

headToParseCodeTableRef :: T.PrimitiveToken -> TeXParser s e m CodeTableRef
headToParseCodeTableRef = \case
    T.CodeTypeTok c ->
        CodeTableRef c <$> parseTeXInt
    _ ->
        P.failure (Just (P.Label ('C' :| "ode table ref"))) mempty

headToParseCharToken :: T.PrimitiveToken -> TeXParser s e m Q.TeXInt
headToParseCharToken = \case
    T.IntRefTok T.CharQuantity c ->
        pure c
    _ ->
        P.failure (Just (P.Label ('C' :| "har token"))) mempty

headToParseMathCharToken :: T.PrimitiveToken -> TeXParser s e m Q.TeXInt
headToParseMathCharToken = \case
    T.IntRefTok T.MathCharQuantity c ->
        pure c
    _ ->
        empty

headToParseFontCharRef :: T.PrimitiveToken -> TeXParser s e m FontCharRef
headToParseFontCharRef = \case
    T.FontCharTok c ->
        FontCharRef c <$> parseHeaded headToParseFontRef
    _ ->
        empty

headToParseInternalTeXInt :: T.PrimitiveToken -> TeXParser s e m InternalTeXInt
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

headToParseFontRef :: T.PrimitiveToken -> TeXParser s e m FontRef
headToParseFontRef =
    choiceFlap
        [ \case
                T.FontTok -> pure CurrentFontRef
                _ -> empty
        , fmap FontTokenRef <$> headToParseFontRefToken
        , fmap FamilyMemberFontRef <$> headToParseFamilyMember
        ]

headToParseFontRefToken :: T.PrimitiveToken -> TeXParser s e m Q.TeXInt
headToParseFontRefToken = \case
    T.FontRefToken n -> pure n
    _                -> empty

headToParseFamilyMember :: T.PrimitiveToken -> TeXParser s e m FamilyMember
headToParseFamilyMember = \case
    T.FontRangeTok r -> FamilyMember r <$> parseTeXInt
    _                -> empty

headToParseInternalLength :: T.PrimitiveToken -> TeXParser s e m InternalLength
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

headToParseSpecialLength :: T.PrimitiveToken -> TeXParser s e m T.SpecialLength
headToParseSpecialLength = \case
    T.SpecialLengthTok p ->
        pure p
    _ ->
        empty

headToParseFontDimensionRef :: T.PrimitiveToken -> TeXParser s e m FontDimensionRef
headToParseFontDimensionRef = \case
    T.FontDimensionTok ->
        FontDimensionRef <$> parseTeXInt <*> parseHeaded headToParseFontRef
    _ ->
        empty

headToParseBoxDimensionRef :: T.PrimitiveToken -> TeXParser s e m BoxDimensionRef
headToParseBoxDimensionRef = \case
    T.BoxDimensionTok dim ->
        do
        boxNr <- parseEightBitTeXInt
        pure $ BoxDimensionRef boxNr dim
    _ ->
        empty

headToParseInternalGlue :: T.PrimitiveToken -> TeXParser s e m InternalGlue
headToParseInternalGlue =
    choiceFlap
        [ fmap InternalGlueVariable <$> headToParseGlueVariable
        , \case
            T.LastGlueTok -> pure LastGlue
            _             -> empty
        ]

headToParseInternalMathGlue :: T.PrimitiveToken -> TeXParser s e m InternalMathGlue
headToParseInternalMathGlue =
    choiceFlap
        [ fmap InternalMathGlueVariable <$> headToParseMathGlueVariable
        , \case
            T.LastGlueTok ->
                pure LastMathGlue
            _ ->
                empty
        ]

headToParseBox :: T.PrimitiveToken -> TeXParser s e m Box
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
        tryChoice
            [ skipKeyword (unsafeCodesFromChars "to") *> (To <$> parseLength)
            , skipKeyword (unsafeCodesFromChars "spread") *> (Spread <$> parseLength)
            , pure Natural
            ] <* skipFiller
