{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.Common where

import Data.Char (chr, toLower, toUpper)
import Data.Functor (($>))
import Data.Maybe (isJust)
import qualified Text.Megaparsec as P

import qualified HeX.Lex as Lex

import HeX.Parse.Helpers
import HeX.Parse.Resolved (PrimitiveToken)
import qualified HeX.Parse.Resolved as R

isTokenForFont :: PrimitiveToken -> Bool
isTokenForFont (R.TokenForFont _) = True
isTokenForFont _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = isCategory Lex.Space

isActiveCharacter :: PrimitiveToken -> Bool
isActiveCharacter = isCategory Lex.Active

isNonActiveCharacter :: PrimitiveToken -> Bool
isNonActiveCharacter = not . isActiveCharacter

isEquals :: PrimitiveToken -> Bool
isEquals (R.CharCat Lex.CharCat {cat = Lex.Other, char = 61}) = True
isEquals _ = False

isCategory :: Lex.LexCatCode -> PrimitiveToken -> Bool
isCategory c (R.CharCat Lex.CharCat {cat = c'}) = c == c'
isCategory _ _ = False

isLetter :: PrimitiveToken -> Bool
isLetter = isCategory Lex.Letter

isOther :: PrimitiveToken -> Bool
isOther = isCategory Lex.Other

isLetterOrOther :: PrimitiveToken -> Bool
isLetterOrOther x = isLetter x || isOther x

isExplicitLeftBrace :: PrimitiveToken -> Bool
isExplicitLeftBrace = isCategory Lex.BeginGroup

matchNonActiveCharacterUncased :: Char -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a t@(R.CharCat Lex.CharCat {char = c}) =
  isNonActiveCharacter t && (chr c `elem` [toUpper a, toLower a])
matchNonActiveCharacterUncased _ _ = False

skipOneOptionalSpace :: (P.Stream s, P.Token s ~ PrimitiveToken) => NullSimpParser s
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: (P.Stream s, P.Token s ~ PrimitiveToken) => NullSimpParser s
skipOptionalSpaces = skipManySatisfied isSpace

skipKeyword :: (P.Stream s, P.Token s ~ PrimitiveToken) => String -> NullSimpParser s
skipKeyword s =
  skipOptionalSpaces *>
  mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: (P.Stream s, P.Token s ~ PrimitiveToken) => String -> SimpParser s Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue :: (P.Stream s, P.Token s ~ PrimitiveToken) => String -> b -> SimpParser s b
parseKeywordToValue s = (skipKeyword s $>)
