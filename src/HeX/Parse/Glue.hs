{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Glue where

import qualified Text.Megaparsec as P

import qualified HeX.Parse.Common as PC
import HeX.Parse.Helpers (skipSatisfied)
import HeX.Parse.Length (Factor, Length, parseFactor, parseLength)
import HeX.Parse.Number (parseSigns)
import HeX.Parse.Stream (SimpExpandParser)

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
    [Just <$> P.try parsePresentFlex, const Nothing <$> PC.skipOptionalSpaces]
  where
    parsePresentFlex =
      PC.skipKeyword s *>
      P.choice
        [FilFlex <$> P.try parseFilLength, FiniteFlex <$> P.try parseLength]

parseFilLength :: SimpExpandParser FilLength
parseFilLength = do
  let parseSomeLs =
        P.some (skipSatisfied $ PC.matchNonActiveCharacterUncased 'l')
      parseOrder = PC.skipKeyword "fi" *> (length <$> parseSomeLs)
  FilLength <$> parseSigns <*> parseFactor <*> parseOrder
