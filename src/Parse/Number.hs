{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Number where

import qualified Text.Megaparsec as P

import qualified Expand
import qualified Lex

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

parseSigns = isPos <$> parseOptionalSigns
  where
    parseOptionalSigns = P.sepEndBy (PU.satisfyThen signToPos) PC.skipOptionalSpaces

    isPos (True:xs) = isPos xs
    isPos (False:xs) = not $ isPos xs
    isPos [] = True

    signToPos (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=43}) = Just True
    signToPos (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=45}) = Just False
    signToPos _  = Nothing

parseNumber = do
  positive <- parseSigns
  uNr <- parseUnsignedNumber
  return $ Number positive uNr

parseUnsignedNumber = P.choice [ NormalIntegerAsUNumber <$> parseNormalInteger
                               -- , CoercedInteger <$> parseCoercedInteger
                               ]

parseNormalInteger = P.choice [ IntegerConstant <$> parseIntegerConstant
                              -- , InternalInteger <$> parseInternalInteger
                              ]

parseIntegerConstant = do
  digits <- P.some parseDigit
  PC.skipOneOptionalSpace
  return $ digitsToInteger 10 digits

parseDigit = PU.satisfyThen charToDigit
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

isDigit x = isOctalDigit x || isEightOrNine x
  where
    isEightOrNine 56 = True
    isEightOrNine 57 = True
    isEightOrNine _ = False

isOctalDigit 48 = True
isOctalDigit 49 = True
isOctalDigit 50 = True
isOctalDigit 51 = True
isOctalDigit 52 = True
isOctalDigit 53 = True
isOctalDigit 54 = True
isOctalDigit 55 = True
isOctalDigit _ = False

-- parseCoercedInteger = P.choice [ parseInternalLengthAsInt
--                                , parseInternalGlueAsInt
--                                ]
