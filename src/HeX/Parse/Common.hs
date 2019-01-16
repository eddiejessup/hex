{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Common where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( isJust )
import qualified Text.Megaparsec               as P

import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex

import           HeX.Parse.Helpers
import           HeX.Parse.Token                ( PrimitiveToken )
import qualified HeX.Parse.Token               as T

-- Helpers.

ccHasCategory :: Lex.LexCatCode -> Lex.CharCat -> Bool
ccHasCategory a Lex.CharCat{cat = b} = a == b

lexTokHasCategory :: Lex.LexCatCode -> Lex.Token -> Bool
lexTokHasCategory a (Lex.CharCatToken cc) = ccHasCategory a cc
lexTokHasCategory _ _                     = False

primTokHasCategory :: Lex.LexCatCode -> PrimitiveToken -> Bool
primTokHasCategory a (T.UnexpandedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _                    = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory Lex.Space

-- Match particular tokens.

isFillerItem :: PrimitiveToken -> Bool
isFillerItem T.RelaxTok = True
isFillerItem t          = isSpace t

isEquals :: PrimitiveToken -> Bool
isEquals (T.UnexpandedTok (Lex.CharCatToken Lex.CharCat{cat = Lex.Other, char = '='})) = True
isEquals _ = False

matchNonActiveCharacterUncased :: Char -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a (T.UnexpandedTok (Lex.CharCatToken Lex.CharCat{char = c, cat=cat})) =
    (cat /= Lex.Active) && (c `elem` [toUpper a, toLower a])
matchNonActiveCharacterUncased _ _ =
    False

tokToChar :: PrimitiveToken -> Maybe CharCode
tokToChar (T.UnexpandedTok (Lex.CharCatToken Lex.CharCat {char = c})) = Just c
tokToChar _ = Nothing

-- Lexed.

tokToLex :: PrimitiveToken -> Maybe Lex.Token
tokToLex (T.UnexpandedTok t) = Just t
tokToLex _                   = Nothing

handleLex
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => (Lex.Token -> Maybe a)
    -> SimpParser s a
handleLex f = satisfyThen $ \x -> tokToLex x >>= f

anySingleLex
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => SimpParser s Lex.Token
anySingleLex = satisfyThen tokToLex

skipSatisfiedEqualsLex :: (P.Stream s, P.Token s ~ PrimitiveToken) => Lex.Token -> NullSimpParser s
skipSatisfiedEqualsLex lt = skipSatisfiedEquals (T.UnexpandedTok lt)

skipSatisfiedLexChunk :: (P.Stream s, P.Token s ~ PrimitiveToken) => [Lex.Token] -> NullSimpParser s
skipSatisfiedLexChunk ts = skipSatisfiedChunk (T.UnexpandedTok <$> ts)

liftLexPred :: (Lex.Token -> Bool) -> PrimitiveToken -> Bool
liftLexPred f (T.UnexpandedTok lt) = f lt
liftLexPred _ _ = False

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
