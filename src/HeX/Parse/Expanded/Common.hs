{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.Common where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( isJust )
import qualified Text.Megaparsec               as P

import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex

import           HeX.Parse.Helpers
import           HeX.Parse.Resolved             ( PrimitiveToken )
import qualified HeX.Parse.Resolved            as R

-- Match categories.

isCategory :: Lex.LexCatCode -> PrimitiveToken -> Bool
isCategory c (R.CharCat Lex.CharCat {cat = c'}) = c == c'
isCategory _ _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = isCategory Lex.Space

isActiveCharacter :: PrimitiveToken -> Bool
isActiveCharacter = isCategory Lex.Active

isNonActiveCharacter :: PrimitiveToken -> Bool
isNonActiveCharacter = not . isActiveCharacter

isLetter :: PrimitiveToken -> Bool
isLetter = isCategory Lex.Letter

isOther :: PrimitiveToken -> Bool
isOther = isCategory Lex.Other

isLetterOrOther :: PrimitiveToken -> Bool
isLetterOrOther x = isLetter x || isOther x

isExplicitLeftBrace :: PrimitiveToken -> Bool
isExplicitLeftBrace = isCategory Lex.BeginGroup

-- Match particular tokens.

isTokenForFont :: PrimitiveToken -> Bool
isTokenForFont (R.TokenForFont _) = True
isTokenForFont _ = False

isFillerItem :: PrimitiveToken -> Bool
isFillerItem R.Relax = True
isFillerItem t       = isSpace t

isEquals :: PrimitiveToken -> Bool
isEquals (R.CharCat Lex.CharCat {cat = Lex.Other, char = '='}) = True
isEquals _ = False

matchNonActiveCharacterUncased :: Char -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a t@(R.CharCat Lex.CharCat {char = c}) =
  isNonActiveCharacter t && (c `elem` [toUpper a, toLower a])
matchNonActiveCharacterUncased _ _ = False

tokToChar :: PrimitiveToken -> Maybe CharCode
tokToChar (R.CharCat Lex.CharCat {char = c}) = Just c
tokToChar _ = Nothing

-- Parsers.

skipOneOptionalSpace :: (P.Stream s, P.Token s ~ PrimitiveToken) => NullSimpParser s
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: (P.Stream s, P.Token s ~ PrimitiveToken) => NullSimpParser s
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: (P.Stream s, P.Token s ~ PrimitiveToken) => NullSimpParser s
skipOptionalEquals = do
  skipOptionalSpaces
  skipOneOptionalSatisfied isEquals

skipKeyword :: (P.Stream s, P.Token s ~ PrimitiveToken) => String -> NullSimpParser s
skipKeyword s =
  skipOptionalSpaces *>
  mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: (P.Stream s, P.Token s ~ PrimitiveToken) => String -> SimpParser s Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue :: (P.Stream s, P.Token s ~ PrimitiveToken) => String -> b -> SimpParser s b
parseKeywordToValue s = (skipKeyword s $>)

parseManyChars :: (P.Stream s, P.Token s ~ PrimitiveToken) => SimpParser s [CharCode]
parseManyChars = P.many $ satisfyThen tokToChar
