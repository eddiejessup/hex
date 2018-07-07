{-# LANGUAGE DuplicateRecordFields #-}

module Parse.Glue where

import qualified Text.Megaparsec as P

import Parse.Util (Parser, skipSatisfied)
import qualified Parse.Common as PC
import Parse.Number (parseSigns)
import Parse.Length (Length, Factor, parseLength, parseFactor)

-- AST.

data Glue
  = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
  -- | InternalGlue Bool InternalGlue
  deriving Show

data Flex
  = FiniteFlex Length
  | FilFlex FilLength
  deriving Show

data FilLength
  = FilLength Bool Factor Int
  deriving Show


-- Parse.

parseGlue :: Parser Glue
parseGlue = P.choice [ parseExplicitGlue
                     -- , parseInternalGlue
                     ]
  where
    parseExplicitGlue = do
      len <- parseLength
      stretch <- parseFlex "plus"
      shrink <- parseFlex "minus"
      return $ ExplicitGlue len stretch shrink

parseFlex :: String -> Parser (Maybe Flex)
parseFlex s = P.choice [ Just <$> parsePresentFlex
                       , const Nothing <$> PC.skipOptionalSpaces ]
  where
    parsePresentFlex = do
      PC.skipKeyword s
      P.choice [ FilFlex <$> parseFilLength
               , FiniteFlex <$> parseLength ]

parseFilLength :: Parser FilLength
parseFilLength = do
  isPositive <- parseSigns
  factor <- parseFactor
  PC.skipKeyword "fi"
  order <- length <$> P.some (skipSatisfied $ PC.matchNonActiveCharacterUncased 'l')
  return $ FilLength isPositive factor order
