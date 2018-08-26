{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.Stream where

import Data.Char (chr, ord, toLower, toUpper)
import Data.Proxy
import qualified Text.Megaparsec as P

import HeX.Categorise (CharCode)
import qualified HeX.Lex as Lex

import HeX.Parse.Helpers

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
  take1_ (ExpandedStream rs) =
    case easyRunParser parseSyntaxCommand rs of
      (_, Left _) -> error "ohnoes"
      (P.State rs' _ _ _, Right v) ->
        case v of
          ChangeCase direction (BalancedText toks) ->
            P.take1_ $
            ExpandedStream $
            R.insertLexTokensR rs' $ changeCase direction <$> toks
          PassPrimitiveToken p -> return (p, ExpandedStream rs')

type SimpExpandParser = P.Parsec () ExpandedStream

insertLexTokenE :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexTokenE (ExpandedStream rs) t = ExpandedStream (insertLexTokenR rs t)

insertLexTokensE :: ExpandedStream -> [Lex.Token] -> ExpandedStream
insertLexTokensE (ExpandedStream rs) ts =
  ExpandedStream (insertLexTokensR rs ts)
