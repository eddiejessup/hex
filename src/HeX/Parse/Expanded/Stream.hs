{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.Stream where

import Data.Char (chr, ord, toLower, toUpper)
import Data.Proxy
import Data.Maybe (isJust)
import Data.Functor (($>))
import qualified Text.Megaparsec as P

import HeX.Categorise (CharCode)
import qualified HeX.Lex as Lex

import HeX.Parse.Helpers
import HeX.Parse.Lexed

import HeX.Parse.Lexed (BalancedText(..))
import HeX.Parse.Resolved (PrimitiveToken)
import HeX.Parse.Resolved as R

newtype ExpandedStream =
  ExpandedStream R.ResolvedStream

newExpandStream :: [CharCode] -> R.CSMap -> ExpandedStream
newExpandStream cs csMap = ExpandedStream $ newResolvedStream cs csMap

type PrimitiveTokens = [PrimitiveToken]

-- Set the character code of each character token to its
-- \uccode or \lccode value, if that value is non-zero.
-- Don't change the category code.
changeCase dir (Lex.CharCatToken (Lex.CharCat char cat)) =
  Lex.CharCatToken $ Lex.CharCat (modChar char) cat
  where
    modChar = ord . switch dir . chr
    switch R.Upward = toUpper
    switch R.Downward = toLower
changeCase _ t = t



parseInhibited :: SimpLexParser a -> SimpExpandParser a
parseInhibited p = do
  P.State {stateInput = ExpandedStream (R.ResolvedStream lStream csMap)} <- P.getParserState
  case easyRunParser p lStream of
    (_, Left _) -> error "ohnoes"
    (P.State lStr pos prc w, Right v) -> do
      P.setParserState (P.State (ExpandedStream $ R.ResolvedStream lStr csMap) pos prc w)
      return v

parseGeneralText :: SimpExpandParser BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  -- TODO: Maybe other things can act as left braces.
  skipSatisfied isExplicitLeftBrace
  parseInhibited parseBalancedText
  where
    isFillerItem R.Relax = True
    isFillerItem t = isSpace t

skipOneOptionalSpace :: NullSimpParser ExpandedStream
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: NullSimpParser ExpandedStream
skipOptionalSpaces = skipManySatisfied isSpace

isTokenForFont :: MatchToken ExpandedStream
isTokenForFont (R.TokenForFont _) = True
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
isEquals (R.CharCat Lex.CharCat {cat = Lex.Other, char = 61}) = True
isEquals _ = False

isCategory :: Lex.LexCatCode -> MatchToken ExpandedStream
isCategory c (R.CharCat Lex.CharCat {cat = c'}) = c == c'
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
matchNonActiveCharacterUncased a t@(R.CharCat Lex.CharCat {char = c}) =
  isNonActiveCharacter t && (chr c `elem` [toUpper a, toLower a])
matchNonActiveCharacterUncased _ _ = False

skipKeyword :: String -> NullSimpParser ExpandedStream
skipKeyword s =
  skipOptionalSpaces *>
  mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: String -> SimpExpandParser Bool
parseOptionalKeyword s = isJust <$> P.optional (P.try $ skipKeyword s)

parseKeywordToValue :: String -> b -> SimpExpandParser b
parseKeywordToValue s = (skipKeyword s $>)


parseChangeCase :: SimpExpandParser BalancedText
parseChangeCase = parseGeneralText






instance P.Stream ExpandedStream where
  type Token ExpandedStream = PrimitiveToken
  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens ExpandedStream = PrimitiveTokens
  -- These basically clarify that, for us, a 'tokens' is a list of type
  -- 'token'.
  -- tokenToChunk :: Proxy s -> Token s -> Tokens s
  -- To make a 'token' into a 'tokens', wrap it in a list.
  tokenToChunk Proxy = pure
  -- tokensToChunk :: Proxy s -> [Token s] -> Tokens s
  -- A list of type 'token' is equivalent to a 'tokens', and vice versa.
  tokensToChunk Proxy = id
  -- chunkToTokens :: Proxy s -> Tokens s -> [Token s]
  chunkToTokens Proxy = id
  -- chunkLength :: Proxy s -> Tokens s -> Int
  -- The length of a chunk is the number of elements in it (it's a list).
  chunkLength Proxy = length
  -- chunkEmpty :: Proxy s -> Tokens s -> Bool
  -- A chunk is empty if it has no elements.
  chunkEmpty Proxy = null
  -- Stub implementation: leave position unchanged.
  advance1 Proxy _ pos _ = pos
  advanceN Proxy _ pos _ = pos
  -- take1_ :: s -> Maybe (Token s, s)
  take1_ (ExpandedStream rs)
    -- Get the next resolved token.
   = do
    (rt, rs') <- P.take1_ rs
    case rt
      -- If it's a primitive token, provide that.
          of
      PrimitiveToken pt -> return (pt, ExpandedStream rs')
      -- If it indicates the start of a syntax command.
      SyntaxCommandHead (ChangeCaseToken direction)
        -- Parse the remainder of the syntax command.
       ->
        case easyRunParser parseChangeCase (ExpandedStream rs') of
          (_, Left _) -> error "ohnoes"
          (P.State es _ _ _, Right (BalancedText caseToks))
            -- Now perform take1_ on the stream after parsing, with the new
            -- tokens inserted.
           ->
            P.take1_ $
            insertLexTokensE es $ changeCase direction <$> caseToks
      SyntaxCommandHead (MacroToken (Macro [] (BalancedText mToks))) ->
            P.take1_ $
            insertLexTokensE (ExpandedStream rs') mToks

type SimpExpandParser = P.Parsec () ExpandedStream

insertLexTokenE :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexTokenE (ExpandedStream rs) t = ExpandedStream (insertLexTokenR rs t)

insertLexTokensE :: ExpandedStream -> [Lex.Token] -> ExpandedStream
insertLexTokensE (ExpandedStream rs) ts =
  ExpandedStream (insertLexTokensR rs ts)
