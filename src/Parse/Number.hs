{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Number where

import qualified Text.Megaparsec as P

import qualified Expand
import qualified Lex

import Parse.Util (Parser)
import qualified Parse.Util as PU
import qualified Parse.Common as PC


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
  = IntegerConstant Int
  deriving Show

-- data CoercedInteger
--   = InternalLengthAsInt InternalLength
--   | InternalGlueAsInt InternalGlue

-- data InternalInteger = TODO
-- data InternalLength = TODO
-- data InternalGlue = TODO


-- Parsing.

digitsToInteger :: Integral n => n -> [n] -> n
digitsToInteger base = foldl (\a b -> a * base + b) 0

parseSigns :: Parser Bool
parseSigns = isPos <$> parseOptionalSigns
  where
    parseOptionalSigns = P.sepEndBy (PU.satisfyThen signToPos) PC.skipOptionalSpaces

    isPos (True:xs) = isPos xs
    isPos (False:xs) = not $ isPos xs
    isPos [] = True

    signToPos (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=43}) = Just True
    signToPos (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=45}) = Just False
    signToPos _  = Nothing

parseNumber :: Parser Number
parseNumber = do
  positive <- parseSigns
  uNr <- parseUnsignedNumber
  return $ Number positive uNr

parseUnsignedNumber :: Parser UnsignedNumber
parseUnsignedNumber = P.choice [ NormalIntegerAsUNumber <$> parseNormalInteger
                               -- , CoercedInteger <$> parseCoercedInteger
                               ]

parseNormalInteger :: Parser NormalInteger
parseNormalInteger = P.choice [ IntegerConstant <$> parseVaribasedIntegerConstant
                              -- , IntegerConstant <$> parseCharacterIntegerConstant
                              -- , InternalInteger <$> parseInternalInteger
                              ]

parseVaribasedIntegerConstant :: Parser Int
parseVaribasedIntegerConstant = do
  (digits, base) <- parseVaribasedIntegerDigits
  PC.skipOneOptionalSpace
  return $ digitsToInteger base digits


parseVaribasedIntegerDigits :: Parser ([Int], Int)
parseVaribasedIntegerDigits = P.choice [ parseDecimalIntegerDigits
                                       , parseHexadecimalIntegerDigits
                                       , parseOctalIntegerDigits
                                       ]

parseDecimalIntegerDigits :: Parser ([Int], Int)
parseDecimalIntegerDigits = do
  digits <- P.some $ PU.satisfyThen charToDigit
  return (digits, 10)
  where
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=48}) = Just 0
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=49}) = Just 1
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=50}) = Just 2
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=51}) = Just 3
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=52}) = Just 4
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=53}) = Just 5
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=54}) = Just 6
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=55}) = Just 7
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=56}) = Just 8
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=57}) = Just 9
    charToDigit _ = Nothing

parseHexadecimalIntegerDigits :: Parser ([Int], Int)
parseHexadecimalIntegerDigits = do
  PU.skipSatisfied isDoubleQuote
  digits <- P.some $ PU.satisfyThen charToDigit
  return (digits, 8)
  where
    isDoubleQuote (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=34}) = True
    isDoubleQuote _ = False

    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=48}) = Just 0
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=49}) = Just 1
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=50}) = Just 2
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=51}) = Just 3
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=52}) = Just 4
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=53}) = Just 5
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=54}) = Just 6
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=55}) = Just 7
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=56}) = Just 8
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=57}) = Just 9
    -- A to F, category 'other',
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=65}) = Just 10
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=66}) = Just 11
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=67}) = Just 12
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=68}) = Just 13
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=69}) = Just 14
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=70}) = Just 15
    -- A to F, category 'letter'.
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=65}) = Just 10
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=66}) = Just 11
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=67}) = Just 12
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=68}) = Just 13
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=69}) = Just 14
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=70}) = Just 15
    charToDigit _ = Nothing

parseOctalIntegerDigits :: Parser ([Int], Int)
parseOctalIntegerDigits = do
  PU.skipSatisfied isSingleQuote
  digits <- P.some $ PU.satisfyThen charToDigit
  return (digits, 8)
  where
    isSingleQuote (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=39}) = True
    isSingleQuote _ = False

    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=48}) = Just 0
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=49}) = Just 1
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=50}) = Just 2
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=51}) = Just 3
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=52}) = Just 4
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=53}) = Just 5
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=54}) = Just 6
    charToDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=55}) = Just 7
    charToDigit _ = Nothing

-- parseCoercedInteger = P.choice [ parseInternalLengthAsInt
--                                , parseInternalGlueAsInt
--                                ]
