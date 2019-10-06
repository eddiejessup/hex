{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module HeX.Parse.Quantity where

import           HeXlude

import qualified Control.Monad.Combinators as PC
import qualified Data.Ascii                as Ascii
import           Data.Foldable             (foldl')
import           Data.Functor              (($>))
import           Data.Ratio                ((%))
import           HeX.Config.Codes          (codesFromStr)
import qualified HeX.Config.Codes          as Code
import qualified HeX.Lex                   as Lex
import           HeX.Parse.AST
import           HeX.Parse.Stream.Class
import qualified HeX.Parse.Token           as T
import qualified HeX.Quantity              as Q

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
parseUnsignedTeXInt = tryChoice [ NormalTeXIntAsUTeXInt <$> parseNormalTeXInt
                                , CoercedTeXInt <$> parseCoercedTeXInt
                                ]

digitsToTeXInt :: Int -> [Int] -> Q.TeXInt
digitsToTeXInt base digs =
    Q.TeXInt $ foldl' (\a b -> a * base + b) 0 digs

parseNormalTeXInt :: TeXParser s e m NormalTeXInt
parseNormalTeXInt =
    tryChoice [ InternalTeXInt <$> parseInternalTeXInt
              , TeXIntConstant <$> parseConstantInt <* skipOneOptionalSpace
              ]
  where
    parseConstantInt = tryChoice
        [ parseConstant
        , Code.toTeXInt <$> parseCharacter ]

    parseConstant =
        tryChoice
            [ digitsToTeXInt 10 <$> PC.some (satisfyThen decCharToInt)
            , digitsToTeXInt 16 <$> (skipSatisfied (matchOtherToken '"') *> PC.some (satisfyThen hexCharToInt))
            , digitsToTeXInt 8  <$> (skipSatisfied (matchOtherToken '\'') *> PC.some (satisfyThen octCharToInt))
            ]

    parseCharacter = skipSatisfied (matchOtherToken '`') *> parseCharLike

decCharToInt :: T.PrimitiveToken -> Maybe Int
decCharToInt = \case
    T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
        Ascii.fromDigit $ Code.codeWord c
    _ ->
        Nothing

hexCharToInt :: T.PrimitiveToken -> Maybe Int
hexCharToInt = \case
    T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
        Ascii.fromUpHexDigit $ Code.codeWord c
    T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Letter)) ->
        Code.safeFromUpAF $ Code.codeWord c
    _ -> Nothing

octCharToInt :: T.PrimitiveToken -> Maybe Int
octCharToInt = \case
    T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
        Ascii.fromOctDigit $ Code.codeWord c
    _ ->
        Nothing

parseCoercedTeXInt :: TeXParser s e m CoercedTeXInt
parseCoercedTeXInt = tryChoice [ InternalLengthAsInt <$> parseInternalLength
                               , InternalGlueAsInt <$> parseInternalGlue
                               ]

-- Length.
parseLength :: TeXParser s e m Length
parseLength = parseSigned parseUnsignedLength

parseUnsignedLength :: TeXParser s e m UnsignedLength
parseUnsignedLength = tryChoice [ NormalLengthAsULength <$> parseNormalLength
                                , CoercedLength <$> parseCoercedLength
                                ]

parseNormalLength :: TeXParser s e m NormalLength
parseNormalLength = tryChoice [ LengthSemiConstant <$> parseFactor <*> parseUnit
                              , InternalLength <$> parseInternalLength
                              ]

parseFactor :: TeXParser s e m Factor

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
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
    tryChoice [ skipOptionalSpaces *> (InternalUnit <$> parseInternalUnit)
             , (PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit)
                   <* skipOneOptionalSpace
             ]
  where
    parseInternalUnit =
        tryChoice [ parseInternalUnitLit <* skipOneOptionalSpace
                  , InternalTeXIntUnit <$> parseInternalTeXInt
                  , InternalLengthUnit <$> parseInternalLength
                  , InternalGlueUnit <$> parseInternalGlue
                  ]

    parseInternalUnitLit =
        tryChoice [ parseKeywordToValue (codesFromStr "em") Em, parseKeywordToValue (codesFromStr "ex") Ex ]

    parseFrame = do
        isTrue <- parseOptionalKeyword (codesFromStr "true")
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
        [ parseKeywordToValue (codesFromStr "bp") Q.BigPoint
        , parseKeywordToValue (codesFromStr "cc") Q.Cicero
        , parseKeywordToValue (codesFromStr "cm") Q.Centimetre
        , parseKeywordToValue (codesFromStr "dd") Q.Didot
        , parseKeywordToValue (codesFromStr "in") Q.Inch
        , parseKeywordToValue (codesFromStr "mm") Q.Millimetre
        , parseKeywordToValue (codesFromStr "pc") Q.Pica
        , parseKeywordToValue (codesFromStr "pt") Q.Point
        , parseKeywordToValue (codesFromStr "sp") Q.ScaledPoint
        ]

parseCoercedLength :: TeXParser s e m CoercedLength
parseCoercedLength = InternalGlueAsLength <$> parseInternalGlue

-- Math length.
parseMathLength :: TeXParser s e m MathLength
parseMathLength = parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: TeXParser s e m UnsignedMathLength
parseUnsignedMathLength =
    tryChoice [ NormalMathLengthAsUMathLength <$> parseNormalMathLength
              , CoercedMathLength <$> parseCoercedMathLength
              ]

parseNormalMathLength :: TeXParser s e m NormalMathLength
parseNormalMathLength =
    MathLengthSemiConstant <$> parseFactor <*> parseMathUnit

parseMathUnit :: TeXParser s e m MathUnit
parseMathUnit =
    tryChoice [ skipKeyword (codesFromStr "mu") >> skipOneOptionalSpace $> Mu
              , skipOptionalSpaces *> (InternalMathGlueAsUnit <$> parseInternalMathGlue)
              ]

parseCoercedMathLength :: TeXParser s e m CoercedMathLength
parseCoercedMathLength = InternalMathGlueAsMathLength <$> parseInternalMathGlue

-- Glue.
parseGlue :: TeXParser s e m Glue
parseGlue = tryChoice [ ExplicitGlue <$> parseLength <*> parseFlex (codesFromStr "plus") <*> parseFlex (codesFromStr "minus")
                      , InternalGlue <$> parseSigned parseInternalGlue
                      ]

parseFlex :: [Code.CharCode] -> TeXParser s e m (Maybe Flex)
parseFlex s = tryChoice [ Just <$> parsePresentFlex
                        , skipOptionalSpaces $> Nothing
                        ]
  where
    parsePresentFlex =
        skipKeyword s
        *> tryChoice [FiniteFlex <$> parseLength, FilFlex <$> parseFilLength]

parseFilLength :: TeXParser s e m FilLength
parseFilLength =
    (FilLength <$> parseSigned parseFactor <*> parseOrder) <* skipOptionalSpaces
  where
    parseSomeLs = PC.some $ skipSatisfied $ matchNonActiveCharacterUncased (Code.codeFromChar 'l')

    parseOrder = skipKeyword (codesFromStr "fi") *> (length <$> parseSomeLs)

-- Math glue.
parseMathGlue :: TeXParser s e m MathGlue
parseMathGlue =
    tryChoice [ ExplicitMathGlue <$> parseMathLength <*> parseMathFlex (codesFromStr "plus") <*> parseMathFlex (codesFromStr "minus")
              , InternalMathGlue <$> parseSigns <*> parseInternalMathGlue
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
parseQuantityVariable
    :: (T.PrimitiveToken -> Maybe p) -- Try to extract a parameter from a token.
    -> T.RegisterType
    -> TeXParser s e m (QuantVariable p)
parseQuantityVariable getParam rTyp =
    tryChoice [ ParamVar <$> satisfyThen getParam
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

parseTeXIntVariable :: TeXParser s e m TeXIntVariable
parseTeXIntVariable = parseQuantityVariable getParam T.RegInt
  where
    getParam (T.IntParamVarTok p) = Just p
    getParam _                    = Nothing

parseLengthVariable :: TeXParser s e m LengthVariable
parseLengthVariable = parseQuantityVariable getParam T.RegLen
  where
    getParam (T.LenParamVarTok p) = Just p
    getParam _                    = Nothing

parseGlueVariable :: TeXParser s e m GlueVariable
parseGlueVariable = parseQuantityVariable getParam T.RegGlue
  where
    getParam (T.GlueParamVarTok p) = Just p
    getParam _                     = Nothing

parseMathGlueVariable :: TeXParser s e m MathGlueVariable
parseMathGlueVariable = parseQuantityVariable getParam T.RegMathGlue
  where
    getParam (T.MathGlueParamVarTok p) = Just p
    getParam _                         = Nothing

parseTokenListVariable :: TeXParser s e m TokenListVariable
parseTokenListVariable = parseQuantityVariable getParam T.RegTokenList
  where
    getParam (T.TokenListParamVarTok p) = Just p
    getParam _                          = Nothing

parseInternalTeXInt :: TeXParser s e m InternalTeXInt
parseInternalTeXInt =
    tryChoice [ InternalTeXIntVariable <$> parseTeXIntVariable
              , InternalSpecialTeXInt <$> parseSpecialTeXInt
              , InternalCodeTableRef <$> parseCodeTableRef
              , InternalCharToken <$> parseCharToken
              , InternalMathCharToken <$> parseMathCharToken
              , InternalFontCharRef <$> parseFontCharRef
              , satisfyEquals T.LastPenaltyTok $> LastPenalty
              , satisfyEquals T.ParagraphShapeTok $> ParShape
              , satisfyEquals T.InputLineNrTok $> InputLineNr
              , satisfyEquals T.BadnessTok $> Badness
              ]

parseCharToken :: TeXParser s e m Q.TeXInt
parseCharToken = satisfyThen $
    \case
        T.IntRefTok T.CharQuantity c -> Just c
        _ -> Nothing

parseMathCharToken :: TeXParser s e m Q.TeXInt
parseMathCharToken = satisfyThen $
    \case
        T.IntRefTok T.MathCharQuantity c -> Just c
        _ -> Nothing

parseSpecialTeXInt :: TeXParser s e m T.SpecialTeXInt
parseSpecialTeXInt = satisfyThen $
    \case
        T.SpecialTeXIntTok p -> Just p
        _ -> Nothing

parseCodeTableRef :: TeXParser s e m CodeTableRef
parseCodeTableRef = CodeTableRef <$> satisfyThen tokToCodeType <*> parseTeXInt
  where
    tokToCodeType (T.CodeTypeTok c) = Just c
    tokToCodeType _                 = Nothing

parseFontCharRef :: TeXParser s e m FontCharRef
parseFontCharRef = FontCharRef <$> satisfyThen tokToFontChar <*> parseFontRef
  where
    tokToFontChar (T.FontCharTok c) = Just c
    tokToFontChar _                 = Nothing

parseFontRef :: TeXParser s e m FontRef
parseFontRef = tryChoice [ FontTokenRef <$> parseFontRefToken
                         , satisfyEquals T.FontTok $> CurrentFontRef
                         , FamilyMemberFontRef <$> parseFamilyMember
                         ]

parseFontRefToken :: TeXParser s e m Q.TeXInt
parseFontRefToken = satisfyThen $
    \case
        T.FontRefToken n -> Just n
        _ -> Nothing

parseFamilyMember :: TeXParser s e m FamilyMember
parseFamilyMember = FamilyMember <$> satisfyThen tokToFontRange
    <*> parseTeXInt
  where
    tokToFontRange (T.FontRangeTok r) = Just r
    tokToFontRange _                  = Nothing

parseInternalLength :: TeXParser s e m InternalLength
parseInternalLength =
    tryChoice [ InternalLengthVariable <$> parseLengthVariable
              , InternalSpecialLength <$> parseSpecialLength
              , InternalFontDimensionRef <$> parseFontDimensionRef
              , InternalBoxDimensionRef <$> parseBoxDimensionRef
              , satisfyEquals T.LastKernTok $> LastKern
              ]

parseSpecialLength :: TeXParser s e m T.SpecialLength
parseSpecialLength = satisfyThen $
    \case
        T.SpecialLengthTok p -> Just p
        _ -> Nothing

parseFontDimensionRef :: TeXParser s e m FontDimensionRef
parseFontDimensionRef = satisfyEquals T.FontDimensionTok
    >> (FontDimensionRef <$> parseTeXInt <*> parseFontRef)

parseBoxDimensionRef :: TeXParser s e m BoxDimensionRef
parseBoxDimensionRef = do
    dim <- parseBoxDimension
    boxNr <- parseEightBitTeXInt
    pure $ BoxDimensionRef boxNr dim

parseBoxDimension :: TeXParser s e m BoxDim
parseBoxDimension = satisfyThen $
    \case
        T.BoxDimensionTok d -> Just d
        _ -> Nothing

parseInternalGlue :: TeXParser s e m InternalGlue
parseInternalGlue = tryChoice [ InternalGlueVariable <$> parseGlueVariable
                              , satisfyEquals T.LastGlueTok $> LastGlue
                              ]

parseInternalMathGlue :: TeXParser s e m InternalMathGlue
parseInternalMathGlue =
    tryChoice [ InternalMathGlueVariable <$> parseMathGlueVariable
              , satisfyEquals T.LastGlueTok $> LastMathGlue
              ]

parseBox :: TeXParser s e m Box
parseBox = tryChoice [ parseRegisterBox
                     , satisfyEquals T.LastBoxTok $> LastBox
                     , parseVSplitBox
                     , parseExplicitBox
                     ]

parseRegisterBox :: TeXParser s e m Box
parseRegisterBox = FetchedRegisterBox <$> parseFetchMode <*> parseEightBitTeXInt
  where
    parseFetchMode = satisfyThen $
        \case
            T.FetchedBoxTok m -> Just m
            _ -> Nothing

parseVSplitBox :: TeXParser s e m Box
parseVSplitBox = do
    satisfyEquals T.SplitVBoxTok
    nr <- parseTeXInt
    skipKeyword (codesFromStr "to")
    VSplitBox nr <$> parseLength

parseExplicitBox :: TeXParser s e m Box
parseExplicitBox = do
    bt <- parseBoxType
    bs <- parseBoxSpecification
    skipLeftBrace
    pure $ ExplicitBox bs bt
  where
    parseBoxSpecification =
        tryChoice [ skipKeyword (codesFromStr "to") *> (To <$> parseLength)
                  , skipKeyword (codesFromStr "spread") *> (Spread <$> parseLength)
                  , pure Natural
                  ]
        <* skipFiller

    parseBoxType = satisfyThen $
        \case
            T.ExplicitBoxTok b -> Just b
            _ -> Nothing
