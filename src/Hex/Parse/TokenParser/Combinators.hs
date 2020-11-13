{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.TokenParser.Combinators where

import qualified Control.Monad.Combinators as PC
import qualified Hex.Config.Codes as Code
import Hex.Lex (CharCat (..))
import qualified Hex.Lex as Lex
import Hex.Parse.TokenParser.Class
import Hex.Resolve
import Hexlude hiding (many)

satisfyIf :: MonadTokenParse m => (PrimitiveToken -> Bool) -> m PrimitiveToken
satisfyIf f = satisfyThen (\x -> if f x then Just x else Nothing)

anySingle :: MonadTokenParse m => m PrimitiveToken
anySingle = satisfyIf (const True)

takeResolvedToken :: MonadTokenParse m => m ResolvedToken
takeResolvedToken = do
  (_, rt) <- takeAndResolveLexToken
  pure rt

manySatisfiedIf :: MonadTokenParse m => (PrimitiveToken -> Bool) -> m [PrimitiveToken]
manySatisfiedIf testTok = PC.many $ satisfyIf testTok

manySatisfiedThen :: MonadTokenParse m => (PrimitiveToken -> Maybe a) -> m [a]
manySatisfiedThen f = PC.many $ satisfyThen f

-- Skipping.
skipSatisfied :: MonadTokenParse m => (PrimitiveToken -> Bool) -> m ()
skipSatisfied f = void (satisfyIf f)

satisfyEquals :: MonadTokenParse m => PrimitiveToken -> m ()
satisfyEquals t = skipSatisfied (== t)

skipOptional :: MonadTokenParse m => m a -> m ()
skipOptional p = void (optional p)

skipOneOptionalSatisfied :: MonadTokenParse m => (PrimitiveToken -> Bool) -> m ()
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: MonadTokenParse m => (PrimitiveToken -> Bool) -> m ()
skipManySatisfied = PC.skipMany . skipSatisfied

skipSatisfiedChunk :: MonadTokenParse m => Seq PrimitiveToken -> m ()
skipSatisfiedChunk = foldr (satisfyEquals >>> (>>)) (pure ())

choiceFlap :: MonadTokenParse m => [PrimitiveToken -> m a] -> PrimitiveToken -> m a
choiceFlap headsToParsers t =
  PC.choice (flap headsToParsers t)

parseHeaded :: MonadTokenParse m => (PrimitiveToken -> m a) -> m a
parseHeaded = (anySingle >>=)

-- Helpers.
ccHasCategory :: Code.CoreCatCode -> CharCat -> Bool
ccHasCategory a CharCat {cat = b} = a == b

lexTokHasCategory :: Code.CoreCatCode -> Lex.Token -> Bool
lexTokHasCategory a (Lex.CharCatToken cc) = ccHasCategory a cc
lexTokHasCategory _ _ = False

primTokHasCategory :: Code.CoreCatCode -> PrimitiveToken -> Bool
primTokHasCategory a (UnresolvedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory Code.Space

-- Match particular tokens.
isFillerItem :: PrimitiveToken -> Bool
isFillerItem = \case
  RelaxTok -> True
  t -> isSpace t

matchOtherToken :: Char -> PrimitiveToken -> Bool
matchOtherToken c2 = \case
  UnresolvedTok (Lex.CharCatToken CharCat {cat = Code.Other, char = c1}) ->
    c1 == Code.CharCode_ c2
  _ ->
    False

matchNonActiveCharacterUncased :: Code.CharCode -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a = \case
  UnresolvedTok (Lex.CharCatToken CharCat {char, cat}) ->
    (cat /= Code.Active) && (char == Code.toUpperChar a || char == Code.toLowerChar a)
  _ ->
    False

tokToChar :: PrimitiveToken -> Maybe Code.CharCode
tokToChar = \case
  UnresolvedTok (Lex.CharCatToken CharCat {char}) ->
    Just char
  _ ->
    Nothing

-- Lexed.
tokToLex :: PrimitiveToken -> Maybe Lex.Token
tokToLex = \case
  UnresolvedTok t -> Just t
  _ -> Nothing

handleLex :: MonadTokenParse m => (Lex.Token -> Maybe a) -> m a
handleLex f = satisfyThen $ tokToLex >=> f

satisfyEqualsLex :: MonadTokenParse m => Lex.Token -> m ()
satisfyEqualsLex lt = void $ satisfyEquals (UnresolvedTok lt)

skipSatisfiedLexChunk :: MonadTokenParse m => Seq Lex.Token -> m ()
skipSatisfiedLexChunk ts = skipSatisfiedChunk (UnresolvedTok <$> ts)

skipBalancedText :: MonadTokenParse m => BalancedText -> m ()
skipBalancedText (BalancedText toks) = skipSatisfiedLexChunk toks

-- Parsers.
skipOneOptionalSpace :: MonadTokenParse m => m ()
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- TODO: Maybe other things can act as left braces.
skipLeftBrace :: MonadTokenParse m => m ()
skipLeftBrace = skipSatisfied $ primTokHasCategory Code.BeginGroup

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: MonadTokenParse m => m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: MonadTokenParse m => m ()
skipOptionalEquals = skipOptionalSpaces >> skipOneOptionalSatisfied (matchOtherToken '=')

skipKeyword :: MonadTokenParse m => [Code.CharCode] -> m ()
skipKeyword s =
  skipOptionalSpaces >>
    mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: MonadTokenParse m => [Code.CharCode] -> m Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

parseManyChars :: MonadTokenParse m => m [Code.CharCode]
parseManyChars = PC.many $ satisfyThen tokToChar
