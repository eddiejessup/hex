{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module HeX.Parse.Expanded.Number where

import Data.Ratio ((%))
import qualified Text.Megaparsec as P

import qualified HeX.Lex as Lex

import HeX.Parse.Lexed
import HeX.Parse.Helpers
import qualified HeX.Parse.Resolved as R

import HeX.Parse.Expanded.Common
import HeX.Parse.Expanded.Stream

-- AST.
data Number =
  Number Bool
         UnsignedNumber
  deriving (Show)

data UnsignedNumber =
  NormalIntegerAsUNumber NormalInteger
  -- | CoercedInteger CoercedInteger
  deriving (Show)

-- Think: 'un-coerced integer'.
data NormalInteger
  -- = InternalInteger InternalInteger
      =
  IntegerConstant Integer
  deriving (Show)-- = InternalLengthAsInt InternalLength
  -- | InternalGlueAsInt InternalGlue

-- data CoercedInteger
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
    parseOptionalSigns =
      skipOptionalSpaces *>
      P.sepEndBy (satisfyThen signToPos) skipOptionalSpaces
    isPos (True:xs) = isPos xs
    isPos (False:xs) = not $ isPos xs
    isPos [] = True
    signToPos (R.CharCat Lex.CharCat {cat = Lex.Other, char = '+'}) =
      Just True
    signToPos (R.CharCat Lex.CharCat {cat = Lex.Other, char = '-'}) =
      Just False
    signToPos _ = Nothing

parseNumber :: SimpExpandParser Number
parseNumber = Number <$> parseSigns <*> parseUnsignedNumber

parseUnsignedNumber :: SimpExpandParser UnsignedNumber
parseUnsignedNumber =
  P.choice
    [ NormalIntegerAsUNumber <$> parseNormalInteger
                               -- , CoercedInteger <$> parseCoercedInteger
    ]

parseNormalInteger :: SimpExpandParser NormalInteger
parseNormalInteger =
  P.choice
    [ IntegerConstant <$> parseConstant
    , IntegerConstant <$> parseCharacter
                              -- , InternalInteger <$> parseInternalInteger
    ]
  where
    parseConstant = do
      (digits, base) <-
        P.choice
          [ (, 10) <$> P.some parseDecimalIntegerDigit
                                 -- , (, 16) <$> parseHexadecimalIntegerDigits
                                 -- , (, 8) <$> parseOctalIntegerDigits
          ]
      skipOneOptionalSpace
      return $ digitsToInteger base digits
    parseCharacter = do
      skipSatisfied isBacktick
      parseInhibited parseCharLike
      where
        isBacktick (R.CharCat Lex.CharCat {cat = Lex.Other, char = '`'}) =
          True
        isBacktick _ = False

parseDecimalIntegerDigit :: SimpExpandParser Int
parseDecimalIntegerDigit = satisfyThen charToDigit
  where
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '0'}) =
      Just 0
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '1'}) =
      Just 1
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '2'}) =
      Just 2
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '3'}) =
      Just 3
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '4'}) =
      Just 4
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '5'}) =
      Just 5
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '6'}) =
      Just 6
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '7'}) =
      Just 7
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '8'}) =
      Just 8
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '9'}) =
      Just 9
    charToDigit _ = Nothing

parseHexadecimalIntegerDigits :: SimpExpandParser [Int]
parseHexadecimalIntegerDigits =
  skipSatisfied isDoubleQuote *> P.some (satisfyThen charToDigit)
  where
    isDoubleQuote (R.CharCat Lex.CharCat {cat = Lex.Other, char = '"'}) =
      True
    isDoubleQuote _ = False
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '0'}) =
      Just 0
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '1'}) =
      Just 1
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '2'}) =
      Just 2
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '3'}) =
      Just 3
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '4'}) =
      Just 4
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '5'}) =
      Just 5
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '6'}) =
      Just 6
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '7'}) =
      Just 7
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '8'}) =
      Just 8
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '9'}) =
      Just 9
    -- A to F, category 'other',
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = 'A'}) =
      Just 10
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = 'B'}) =
      Just 11
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = 'C'}) =
      Just 12
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = 'D'}) =
      Just 13
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = 'E'}) =
      Just 14
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = 'F'}) =
      Just 15
    -- A to F, category 'letter'.
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Letter, char = 'A'}) =
      Just 10
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Letter, char = 'B'}) =
      Just 11
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Letter, char = 'C'}) =
      Just 12
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Letter, char = 'D'}) =
      Just 13
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Letter, char = 'E'}) =
      Just 14
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Letter, char = 'F'}) =
      Just 15
    charToDigit _ = Nothing

parseOctalIntegerDigits :: SimpExpandParser [Int]
parseOctalIntegerDigits =
  skipSatisfied isSingleQuote *> P.some (satisfyThen charToDigit)
  where
    isSingleQuote (R.CharCat Lex.CharCat {cat = Lex.Other, char = '\''}) =
      True
    isSingleQuote _ = False
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '0'}) =
      Just 0
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '1'}) =
      Just 1
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '2'}) =
      Just 2
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '3'}) =
      Just 3
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '4'}) =
      Just 4
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '5'}) =
      Just 5
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '6'}) =
      Just 6
    charToDigit (R.CharCat Lex.CharCat {cat = Lex.Other, char = '7'}) =
      Just 7
    charToDigit _ = Nothing

-- parseCoercedInteger = P.choice [ parseInternalLengthAsInt
--                                , parseInternalGlueAsInt
--                                ]
parseRationalConstant :: SimpExpandParser Rational
parseRationalConstant = do
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
  return $ fromIntegral wholeNr + fraction
  where
    decDigitsToInteger = digitsToInteger 10
    isDotOrComma (R.CharCat Lex.CharCat {cat = Lex.Other, char = ','}) =
      True
    isDotOrComma (R.CharCat Lex.CharCat {cat = Lex.Other, char = '.'}) =
      True
    isDotOrComma _ = False
