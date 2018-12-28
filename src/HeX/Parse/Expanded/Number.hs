{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module HeX.Parse.Expanded.Number where

import           Data.Ratio                     ( (%) )
import qualified Text.Megaparsec               as P

import qualified HeX.Lex                       as Lex

import           HeX.Parse.Lexed
import           HeX.Parse.Helpers
import qualified HeX.Parse.Resolved            as R

import           HeX.Parse.Expanded.Common
import           HeX.Parse.Expanded.Stream

-- AST.
data Number = Number Bool UnsignedNumber
  deriving (Show)

data UnsignedNumber
  = NormalIntegerAsUNumber NormalInteger
  -- \| CoercedInteger CoercedInteger
  deriving (Show)

-- Think: 'un-coerced integer'.
data NormalInteger
  = IntegerConstant Integer
  -- | InternalInteger InternalInteger
  deriving (Show)-- = InternalLengthAsInt InternalLength
  -- \| InternalGlueAsInt InternalGlue

-- data CoercedInteger
-- data InternalInteger = TODO
-- data InternalLength = TODO
-- data InternalGlue = TODO
-- Parsing.
-- Restrict pure type, and therefore accumulator, to Integer, to disallow
-- overflow.
digitsToInteger :: Integral n => n -> [n] -> Integer
digitsToInteger base = foldl (\a b -> a * fromIntegral base + fromIntegral b) 0

-- mconcat on this newtype wrapper should get the final sign of a list of
-- signs. Bit pretentious, sorry.
newtype Sign = Sign { getSign :: Bool }
  deriving (Show, Eq)

instance Semigroup Sign where
  Sign x <> Sign y = Sign $ x == y

instance Monoid Sign where
  mempty = Sign True

parseSigns :: SimpExpandParser Bool
parseSigns = getSign . mconcat <$> parseOptionalSigns
 where
  parseOptionalSigns =
    skipOptionalSpaces *> P.sepEndBy (satisfyThen signToPos) skipOptionalSpaces
  signToPos (R.CharCat (Lex.CharCat '+' Lex.Other)) = Just $ Sign True
  signToPos (R.CharCat (Lex.CharCat '-' Lex.Other)) = Just $ Sign False
  signToPos _ = Nothing

parseNumber :: SimpExpandParser Number
parseNumber = Number <$> parseSigns <*> parseUnsignedNumber

parseUnsignedNumber :: SimpExpandParser UnsignedNumber
parseUnsignedNumber =
  P.choice [NormalIntegerAsUNumber <$> parseNormalInteger
                               -- , CoercedInteger <$> parseCoercedInteger
                                                         ]

parseNormalInteger :: SimpExpandParser NormalInteger
parseNormalInteger = P.choice
  [ IntegerConstant <$> parseConstant
  , IntegerConstant <$> parseCharacter
                              -- , InternalInteger <$> parseInternalInteger
  ]
 where
  parseConstant = do
    (digits, base) <- P.choice [(, 10) <$> P.some parseDecimalIntegerDigit
                               -- , (, 16) <$> parseHexadecimalIntegerDigits
                               -- , (, 8) <$> parseOctalIntegerDigits
                                                                          ]
    skipOneOptionalSpace
    pure $ digitsToInteger base digits
  parseCharacter = do
    skipSatisfied isBacktick
    parseInhibited parseCharLike
   where
    isBacktick (R.CharCat (Lex.CharCat '`' Lex.Other)) = True
    isBacktick _ = False

parseDecimalIntegerDigit :: SimpExpandParser Int
parseDecimalIntegerDigit = satisfyThen decCharToInt
 where
  decCharToInt (R.CharCat (Lex.CharCat c Lex.Other)) = case c of
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
parseHexadecimalIntegerDigits = skipSatisfied isDoubleQuote
  *> P.some (satisfyThen hexCharToInt)
 where
  isDoubleQuote (R.CharCat (Lex.CharCat '"' Lex.Other)) = True
  isDoubleQuote _ = False

  hexCharToInt (R.CharCat (Lex.CharCat c Lex.Other)) = case c of
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
  hexCharToInt (R.CharCat (Lex.CharCat c Lex.Letter)) = case c of
    'A' -> Just 10
    'B' -> Just 11
    'C' -> Just 12
    'D' -> Just 13
    'E' -> Just 14
    'F' -> Just 15
    _   -> Nothing
  hexCharToInt _ = Nothing

parseOctalIntegerDigits :: SimpExpandParser [Int]
parseOctalIntegerDigits = skipSatisfied isSingleQuote
  *> P.some (satisfyThen octCharToInt)
 where
  isSingleQuote (R.CharCat (Lex.CharCat '\'' Lex.Other)) = True
  isSingleQuote _ = False

  octCharToInt (R.CharCat (Lex.CharCat c Lex.Other)) = case c of
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
  pure $ fromIntegral wholeNr + fraction
 where
  decDigitsToInteger = digitsToInteger 10
  isDotOrComma (R.CharCat (Lex.CharCat ',' Lex.Other)) = True
  isDotOrComma (R.CharCat (Lex.CharCat '.' Lex.Other)) = True
  isDotOrComma _ = False
