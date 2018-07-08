{-# LANGUAGE DuplicateRecordFields #-}

module Parse.Common where

import qualified Text.Megaparsec as P

import qualified Expand
import qualified Lex
import qualified Categorise as Cat
import qualified Data.Char as C
import Data.Maybe (isJust)

import Parse.Util (MatchToken, skipManySatisfied, skipOneOptionalSatisfied, Parser, NullParser)
import qualified Parse.Util as PU

data ControlSequenceLike = ActiveCharacter Cat.CharCode | ControlSequence Lex.ControlSequence
  deriving Show

skipOneOptionalSpace :: NullParser
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: NullParser
skipOptionalSpaces = skipManySatisfied isSpace

isLetterOrOther :: MatchToken
isLetterOrOther x = isLetter x || isOther x

isTokenForFont :: MatchToken
isTokenForFont (Expand.TokenForFont _) = True
isTokenForFont _ = False

isUnexpandedControlSequence :: MatchToken
isUnexpandedControlSequence (Expand.UnexpandedControlSequence _) = True
isUnexpandedControlSequence _ = False

isActiveCharacter :: MatchToken
isActiveCharacter (Expand.CharCat Lex.LexCharCat{cat=Lex.Active}) = True
isActiveCharacter _ = False

isNonActiveCharacter :: MatchToken
isNonActiveCharacter = not . isActiveCharacter

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: MatchToken
isSpace (Expand.CharCat Lex.LexCharCat{cat=Lex.Space}) = True
isSpace _ = False

isEquals :: MatchToken
isEquals (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=61}) = True
isEquals _ = False

isLetter :: MatchToken
isLetter (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter}) = True
isLetter _ = False

isOther :: MatchToken
isOther (Expand.CharCat Lex.LexCharCat{cat=Lex.Other}) = True
isOther _ = False

matchNonActiveCharacterUncased :: Char -> MatchToken
matchNonActiveCharacterUncased a t@(Expand.CharCat Lex.LexCharCat{char=c})
  = isNonActiveCharacter t && (C.chr c `elem` [C.toUpper a, C.toLower a])
matchNonActiveCharacterUncased _ _ = False

skipKeyword :: String -> NullParser
skipKeyword s = do
  skipOptionalSpaces
  mapM_ (PU.skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: String -> Parser Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue :: String -> b -> Parser b
parseKeywordToValue s c = do
  skipKeyword s
  return c

parseCSName :: Parser ControlSequenceLike
parseCSName = do
  PU.disableExpansion
  csLike <- PU.satisfyThen parseCSLike
  PU.enableExpansion
  return csLike
  where
    parseCSLike (Expand.CharCat Lex.LexCharCat{cat=Lex.Active, char=c}) = Just $ ActiveCharacter c
    parseCSLike (Expand.UnexpandedControlSequence cs) = Just $ ControlSequence cs
    parseCSLike _ = Nothing
