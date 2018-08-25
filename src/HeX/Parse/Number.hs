{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module HeX.Parse.Number where

import qualified Text.Megaparsec as P
import Data.Ratio ((%))

import qualified HeX.Expand as Expand
import qualified HeX.Lex as Lex

import HeX.Parse.Stream (SimpExpandParser, SimpExpandParser)
import qualified HeX.Parse.Helpers as PU
import qualified HeX.Parse.Common as PC
import HeX.Parse.Inhibited (parseInhibited, parseCharLike)

-- AST.

data Number
  = Number Bool UnsignedNumber
  deriving Show

data UnsignedNumber
  = NormalIntegerAsUNumber NormalInteger
  -- | CoercedInteger CoercedInteger
  deriving Show

-- Think: 'un-coerced integer'.
data NormalInteger
  -- = InternalInteger InternalInteger
  = IntegerConstant Integer
  deriving Show

-- data CoercedInteger
  -- = InternalLengthAsInt InternalLength
  -- | InternalGlueAsInt InternalGlue

-- data InternalInteger = TODO
-- data InternalLength = TODO
-- data InternalGlue = TODO


-- Parsing.

-- Restrict return type, and therefore accumulator, to Integer, to disallow
-- overflow.
digitsToInteger :: Integral n => n -> [n] -> Integer
digitsToInteger base = foldl (\a b -> a * fromIntegral base + fromIntegral b) 0

parseSigns :: SimpExpandParser Bool
parseSigns = isPos <$> parseOptionalSigns
  where
    parseOptionalSigns
      = PC.skipOptionalSpaces *> P.sepEndBy (PU.satisfyThen signToPos) PC.skipOptionalSpaces

    isPos (True:xs) = isPos xs
    isPos (False:xs) = not $ isPos xs
    isPos [] = True

    signToPos (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=43}) = Just True
    signToPos (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=45}) = Just False
    signToPos _  = Nothing

parseNumber :: SimpExpandParser Number
parseNumber = Number <$> parseSigns <*> parseUnsignedNumber

parseUnsignedNumber :: SimpExpandParser UnsignedNumber
parseUnsignedNumber = P.choice [ NormalIntegerAsUNumber <$> parseNormalInteger
                               -- , CoercedInteger <$> parseCoercedInteger
                               ]

parseNormalInteger :: SimpExpandParser NormalInteger
parseNormalInteger = P.choice [ IntegerConstant <$> parseConstant
                              , IntegerConstant <$> parseCharacter
                              -- , InternalInteger <$> parseInternalInteger
                              ]
  where
    parseConstant = do
      (digits, base) <- P.choice [ (, 10) <$> P.some parseDecimalIntegerDigit
                                 -- , (, 16) <$> parseHexadecimalIntegerDigits
                                 -- , (, 8) <$> parseOctalIntegerDigits
                                 ]
      PC.skipOneOptionalSpace
      return $ digitsToInteger base digits
    parseCharacter = do
      PU.skipSatisfied isBacktick
      parseInhibited parseCharLike
      where
        isBacktick (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=96}) = True
        isBacktick _ = False

parseDecimalIntegerDigit :: SimpExpandParser Int
parseDecimalIntegerDigit = PU.satisfyThen charToDigit
  where
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=48}) = Just 0
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=49}) = Just 1
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=50}) = Just 2
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=51}) = Just 3
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=52}) = Just 4
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=53}) = Just 5
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=54}) = Just 6
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=55}) = Just 7
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=56}) = Just 8
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=57}) = Just 9
    charToDigit _ = Nothing

parseHexadecimalIntegerDigits :: SimpExpandParser [Int]
parseHexadecimalIntegerDigits =
  PU.skipSatisfied isDoubleQuote *> P.some (PU.satisfyThen charToDigit)
  where
    isDoubleQuote (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=34}) = True
    isDoubleQuote _ = False

    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=48}) = Just 0
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=49}) = Just 1
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=50}) = Just 2
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=51}) = Just 3
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=52}) = Just 4
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=53}) = Just 5
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=54}) = Just 6
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=55}) = Just 7
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=56}) = Just 8
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=57}) = Just 9
    -- A to F, category 'other',
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=65}) = Just 10
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=66}) = Just 11
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=67}) = Just 12
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=68}) = Just 13
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=69}) = Just 14
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=70}) = Just 15
    -- A to F, category 'letter'.
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=65}) = Just 10
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=66}) = Just 11
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=67}) = Just 12
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=68}) = Just 13
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=69}) = Just 14
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=70}) = Just 15
    charToDigit _ = Nothing

parseOctalIntegerDigits :: SimpExpandParser [Int]
parseOctalIntegerDigits =
  PU.skipSatisfied isSingleQuote *> P.some (PU.satisfyThen charToDigit)
  where
    isSingleQuote (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=39}) = True
    isSingleQuote _ = False

    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=48}) = Just 0
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=49}) = Just 1
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=50}) = Just 2
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=51}) = Just 3
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=52}) = Just 4
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=53}) = Just 5
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=54}) = Just 6
    charToDigit (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=55}) = Just 7
    charToDigit _ = Nothing

-- parseCoercedInteger = P.choice [ parseInternalLengthAsInt
--                                , parseInternalGlueAsInt
--                                ]

parseRationalConstant :: SimpExpandParser Rational
parseRationalConstant = do
  wholeNr <- decDigitsToInteger <$> P.many parseDecimalIntegerDigit
  PU.skipSatisfied isDotOrComma

  -- The fractional part represents its integer interpretation, divided by
  -- the next largest power of 10.
  -- TODO: If performance matters, maybe we can infer the denominator
  -- faster from the integer itself than the digits.
  fracDigits <- P.many parseDecimalIntegerDigit
  let fraction = fromIntegral (decDigitsToInteger fracDigits) % (10 ^ length fracDigits)

  -- Convert the whole number to a rational, and add it to the fraction.
  return $ fromIntegral wholeNr + fraction
  where
    decDigitsToInteger = digitsToInteger 10

    isDotOrComma (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=44}) = True
    isDotOrComma (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=46}) = True
    isDotOrComma _ = False
