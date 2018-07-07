{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Common where

import qualified Text.Megaparsec as P

import qualified Expand
import qualified Lex
import qualified Data.Char as C
import Data.Maybe (isJust)

import Parse.Util (MatchToken, skipManySatisfied, skipOneOptionalSatisfied, Parser, NullParser)
import qualified Parse.Util as PU

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

parseKeyword :: String -> NullParser
parseKeyword s = do
  skipOptionalSpaces
  mapM_ (PU.skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: String -> Parser Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ parseKeyword s)

parseKeywordToValue :: String -> b -> Parser b
parseKeywordToValue s c = do
  parseKeyword s
  return c
