{-# LANGUAGE DuplicateRecordFields #-}

module Parse.Common where

import qualified Text.Megaparsec as P

import qualified Expand
import qualified Lex
import qualified Categorise as Cat
import qualified Data.Char as C
import Data.Maybe (isJust)
import Data.Functor (($>))

import Parse.Util (MatchToken, skipManySatisfied, skipOneOptionalSatisfied, Parser, NullParser)
import qualified Parse.Util as PU

data ControlSequenceLike = ActiveCharacter Cat.CharCode | ControlSequence Lex.ControlSequence
  deriving Show

newtype BalancedText = BalancedText [Lex.Token]
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
isUnexpandedControlSequence (Expand.LexToken (Lex.ControlSequence _)) = True
isUnexpandedControlSequence _ = False

isActiveCharacter :: MatchToken
isActiveCharacter (Expand.LexToken Lex.CharCat{cat=Lex.Active}) = True
isActiveCharacter _ = False

isNonActiveCharacter :: MatchToken
isNonActiveCharacter = not . isActiveCharacter

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: MatchToken
isSpace (Expand.LexToken Lex.CharCat{cat=Lex.Space}) = True
isSpace _ = False

isEquals :: MatchToken
isEquals (Expand.LexToken Lex.CharCat{cat=Lex.Other, char=61}) = True
isEquals _ = False

isLetter :: MatchToken
isLetter (Expand.LexToken Lex.CharCat{cat=Lex.Letter}) = True
isLetter _ = False

isOther :: MatchToken
isOther (Expand.LexToken Lex.CharCat{cat=Lex.Other}) = True
isOther _ = False

matchNonActiveCharacterUncased :: Char -> MatchToken
matchNonActiveCharacterUncased a t@(Expand.LexToken Lex.CharCat{char=c})
  = isNonActiveCharacter t && (C.chr c `elem` [C.toUpper a, C.toLower a])
matchNonActiveCharacterUncased _ _ = False

skipKeyword :: String -> NullParser
skipKeyword s
  = skipOptionalSpaces *> mapM_ (PU.skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: String -> Parser Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue :: String -> b -> Parser b
parseKeywordToValue s = (skipKeyword s $>)

parseWithoutExpansion :: Parser a -> Parser a
parseWithoutExpansion p = do
  PU.disableExpansion
  r <- p
  PU.enableExpansion
  return r

parseCSName :: Parser ControlSequenceLike
parseCSName = parseWithoutExpansion $ PU.satisfyThen tokToCSLike
  where
    tokToCSLike (Expand.LexToken Lex.CharCat{cat=Lex.Active, char=c}) = Just $ ActiveCharacter c
    tokToCSLike (Expand.LexToken (Lex.ControlSequence cs)) = Just $ ControlSequence cs
    tokToCSLike _ = Nothing

parseNestedBraces :: Int -> Parser [Lex.Token]
parseNestedBraces 0 = return []
parseNestedBraces n = do
  (x, nextN) <- PU.satisfyThen parseNext
  case nextN of
    0 -> return []
    posN -> (x:) <$> parseNestedBraces posN
  where
    parseNext (Expand.LexToken x@Lex.CharCat{cat=Lex.BeginGroup}) = Just (x, succ n)
    parseNext (Expand.LexToken x@Lex.CharCat{cat=Lex.EndGroup}) = Just (x, pred n)
    parseNext (Expand.LexToken x) = Just (x, n)
    -- We expect to call this with expansion disabled, so only lex tokens
    -- should be seen.
    parseNext _ = Nothing

parseGeneralText :: Parser BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  PU.skipSatisfied isLeftBrace
  BalancedText <$> parseWithoutExpansion (parseNestedBraces 1)
  where
    isFillerItem Expand.Relax = True
    isFillerItem t = isSpace t

    -- TODO: Maybe other things can act as left braces.
    isLeftBrace (Expand.LexToken Lex.CharCat{cat=Lex.BeginGroup}) = True
    isLeftBrace _ = False
