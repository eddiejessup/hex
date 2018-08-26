{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Expanded.Glue where

import qualified Text.Megaparsec as P

import HeX.Parse.Helpers

import HeX.Parse.Expanded.Common
import HeX.Parse.Expanded.Length
import HeX.Parse.Expanded.Number
import HeX.Parse.Expanded.Stream

-- AST.
data Glue =
  ExplicitGlue Length
               (Maybe Flex)
               (Maybe Flex)
  -- | InternalGlue Bool InternalGlue
  deriving (Show)

data Flex
  = FiniteFlex Length
  | FilFlex FilLength
  deriving (Show)

data FilLength =
  FilLength Bool
            Factor
            Int
  deriving (Show)

-- Parse.
parseGlue :: SimpExpandParser Glue
parseGlue =
  P.choice
    [ parseExplicitGlue
 -- , parseInternalGlue
    ]
  where
    parseExplicitGlue =
      ExplicitGlue <$> parseLength <*> parseFlex "plus" <*> parseFlex "minus"

parseFlex :: String -> SimpExpandParser (Maybe Flex)
parseFlex s =
  P.choice
    [Just <$> P.try parsePresentFlex, const Nothing <$> skipOptionalSpaces]
  where
    parsePresentFlex =
      skipKeyword s *>
      P.choice
        [FilFlex <$> P.try parseFilLength, FiniteFlex <$> P.try parseLength]

parseFilLength :: SimpExpandParser FilLength
parseFilLength = do
  let parseSomeLs =
        P.some (skipSatisfied $ matchNonActiveCharacterUncased 'l')
      parseOrder = skipKeyword "fi" *> (length <$> parseSomeLs)
  FilLength <$> parseSigns <*> parseFactor <*> parseOrder
