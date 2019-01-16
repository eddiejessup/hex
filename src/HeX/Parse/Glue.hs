{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Glue where

import qualified Text.Megaparsec               as P

import           HeX.Parse.Helpers
import           HeX.Parse.AST
import           HeX.Parse.Common
import           HeX.Parse.Length
import           HeX.Parse.Number
import           HeX.Parse.Stream

parseGlue :: SimpExpandParser Glue
parseGlue = P.choice [ parseExplicitGlue
                     -- , parseInternalGlue
                     ]
  where
    parseExplicitGlue =
      ExplicitGlue <$> parseLength <*> parseFlex "plus" <*> parseFlex "minus"

parseFlex :: String -> SimpExpandParser (Maybe Flex)
parseFlex s = P.choice [ Just <$> P.try parsePresentFlex
                       , const Nothing <$> skipOptionalSpaces
                       ]
  where
    parsePresentFlex = skipKeyword s *> P.choice [ FilFlex <$> P.try parseFilLength
                                                 , FiniteFlex <$> P.try parseLength
                                                 ]

parseFilLength :: SimpExpandParser FilLength
parseFilLength =
    let parseSomeLs = P.some $ skipSatisfied $ matchNonActiveCharacterUncased 'l'
        parseOrder = skipKeyword "fi" *> (length <$> parseSomeLs)
    in FilLength <$> parseSigns <*> parseFactor <*> parseOrder
