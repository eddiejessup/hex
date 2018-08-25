{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Common where

import qualified Data.Char as C
import Data.Functor (($>))
import Data.Maybe (isJust)
import qualified Text.Megaparsec as P

import qualified HeX.Expand as Expand
import qualified HeX.Lex as Lex

import HeX.Parse.Helpers
       (MatchToken, NullSimpParser, NullSimpParser, skipManySatisfied,
        skipOneOptionalSatisfied)
import qualified HeX.Parse.Helpers as PU
import HeX.Parse.Stream (ExpandedStream(..), SimpExpandParser)

skipOneOptionalSpace :: NullSimpParser ExpandedStream
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: NullSimpParser ExpandedStream
skipOptionalSpaces = skipManySatisfied isSpace

isTokenForFont :: MatchToken ExpandedStream
isTokenForFont (Expand.TokenForFont _) = True
isTokenForFont _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: MatchToken ExpandedStream
isSpace = isCategory Lex.Space

isActiveCharacter :: MatchToken ExpandedStream
isActiveCharacter = isCategory Lex.Active

isNonActiveCharacter :: MatchToken ExpandedStream
isNonActiveCharacter = not . isActiveCharacter

isEquals :: MatchToken ExpandedStream
isEquals (Expand.CharCat Lex.CharCat {cat = Lex.Other, char = 61}) = True
isEquals _ = False

isCategory :: Lex.LexCatCode -> MatchToken ExpandedStream
isCategory c (Expand.CharCat Lex.CharCat {cat = c'}) = c == c'
isCategory _ _ = False

isLetter :: MatchToken ExpandedStream
isLetter = isCategory Lex.Letter

isOther :: MatchToken ExpandedStream
isOther = isCategory Lex.Other

isLetterOrOther :: MatchToken ExpandedStream
isLetterOrOther x = isLetter x || isOther x

isExplicitLeftBrace :: MatchToken ExpandedStream
isExplicitLeftBrace = isCategory Lex.BeginGroup

matchNonActiveCharacterUncased :: Char -> MatchToken ExpandedStream
matchNonActiveCharacterUncased a t@(Expand.CharCat Lex.CharCat {char = c}) =
  isNonActiveCharacter t && (C.chr c `elem` [C.toUpper a, C.toLower a])
matchNonActiveCharacterUncased _ _ = False

skipKeyword :: String -> NullSimpParser ExpandedStream
skipKeyword s =
  skipOptionalSpaces *>
  mapM_ (PU.skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: String -> SimpExpandParser Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue :: String -> b -> SimpExpandParser b
parseKeywordToValue s = (skipKeyword s $>)
