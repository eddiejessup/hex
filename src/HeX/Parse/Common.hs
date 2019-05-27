module HeX.Parse.Common where

import           HeXlude

import           Data.Char         ( toLower, toUpper )
import           Data.Functor      ( ($>) )
import           Data.Maybe        ( isJust )

import qualified Text.Megaparsec   as P

import           HeX.Categorise    ( CharCode )
import           HeX.Lex           ( CharCat(..), Token(..) )
import qualified HeX.Lex           as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Token   ( PrimitiveToken )
import qualified HeX.Parse.Token   as T

-- Helpers.
ccHasCategory :: Lex.LexCatCode -> CharCat -> Bool
ccHasCategory a CharCat{cat = b} = a == b

lexTokHasCategory :: Lex.LexCatCode -> Token -> Bool
lexTokHasCategory a (CharCatToken cc) = ccHasCategory a cc
lexTokHasCategory _ _ = False

primTokHasCategory :: Lex.LexCatCode -> PrimitiveToken -> Bool
primTokHasCategory a (T.UnexpandedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory Lex.Space

-- Match particular tokens.
isFillerItem :: PrimitiveToken -> Bool
isFillerItem T.RelaxTok = True
isFillerItem t = isSpace t

matchOtherToken :: CharCode -> PrimitiveToken -> Bool
matchOtherToken c2
                (T.UnexpandedTok (CharCatToken CharCat{ cat = Lex.Other
                                                      , char = c1
                                                      })) = c1 == c2
matchOtherToken _ _ = False

isEquals :: PrimitiveToken -> Bool
isEquals = matchOtherToken '='

matchNonActiveCharacterUncased :: Char -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a
                               (T.UnexpandedTok (CharCatToken CharCat{ char = c
                                                                     , cat = cat
                                                                     })) =
    (cat /= Lex.Active) && (c `elem` [ toUpper a, toLower a ])
matchNonActiveCharacterUncased _ _ = False

tokToChar :: PrimitiveToken -> Maybe CharCode
tokToChar (T.UnexpandedTok (CharCatToken CharCat{char = c})) = Just c
tokToChar _ = Nothing

-- Lexed.
tokToLex :: PrimitiveToken -> Maybe Token
tokToLex (T.UnexpandedTok t) = Just t
tokToLex _ = Nothing

handleLex :: (P.Stream s, P.Token s ~ PrimitiveToken)
          => (Token -> Maybe a)
          -> SimpParser s a
handleLex f = satisfyThen $ tokToLex >=> f

skipSatisfiedEqualsLex
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => Token
    -> NullSimpParser s
skipSatisfiedEqualsLex lt = skipSatisfiedEquals (T.UnexpandedTok lt)

skipSatisfiedLexChunk :: (P.Stream s, P.Token s ~ PrimitiveToken)
                      => [Token]
                      -> NullSimpParser s
skipSatisfiedLexChunk ts = skipSatisfiedChunk (T.UnexpandedTok <$> ts)

skipBalancedText :: (P.Stream s, P.Token s ~ PrimitiveToken)
                 => T.BalancedText
                 -> NullSimpParser s
skipBalancedText (T.BalancedText toks) = skipSatisfiedLexChunk toks

liftLexPred :: (Token -> Bool) -> PrimitiveToken -> Bool
liftLexPred f (T.UnexpandedTok lt) = f lt
liftLexPred _ _ = False

-- Parsers.
skipOneOptionalSpace :: (P.Stream s, P.Token s ~ PrimitiveToken)
                     => NullSimpParser s
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- TODO: Maybe other things can act as left braces.
skipLeftBrace :: (P.Stream s, P.Token s ~ PrimitiveToken) => NullSimpParser s
skipLeftBrace = skipSatisfied $ primTokHasCategory Lex.BeginGroup

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: (P.Stream s, P.Token s ~ PrimitiveToken)
                   => NullSimpParser s
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: (P.Stream s, P.Token s ~ PrimitiveToken)
                   => NullSimpParser s
skipOptionalEquals = do
    skipOptionalSpaces
    skipOneOptionalSatisfied isEquals

skipKeyword :: (P.Stream s, P.Token s ~ PrimitiveToken)
            => [CharCode]
            -> NullSimpParser s
skipKeyword s = skipOptionalSpaces
    *> mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: (P.Stream s, P.Token s ~ PrimitiveToken)
                     => [CharCode]
                     -> SimpParser s Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => [CharCode]
    -> b
    -> SimpParser s b
parseKeywordToValue s = (skipKeyword s $>)

parseManyChars :: (P.Stream s, P.Token s ~ PrimitiveToken)
               => SimpParser s [CharCode]
parseManyChars = P.many $ satisfyThen tokToChar
