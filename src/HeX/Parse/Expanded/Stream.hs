{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.Stream where

import Data.Char (chr, ord, toLower, toUpper)
import Data.Proxy
import qualified Text.Megaparsec as P

import HeX.Categorise (CharCode)
import qualified HeX.Lex as Lex

import HeX.Parse.Helpers
import HeX.Parse.Lexed

import HeX.Parse.Resolved (PrimitiveToken)
import HeX.Parse.Resolved as R

import HeX.Parse.Expanded.Common

newtype ExpandedStream =
  ExpandedStream R.ResolvedStream

newExpandStream :: [CharCode] -> R.CSMap -> ExpandedStream
newExpandStream cs csMap = ExpandedStream $ newResolvedStream cs csMap

type PrimitiveTokens = [PrimitiveToken]

-- Set the character code of each character token to its
-- \uccode or \lccode value, if that value is non-zero.
-- Don't change the category code.
changeCase :: VDirection -> Lex.Token -> Lex.Token
changeCase dir (Lex.CharCatToken (Lex.CharCat char cat)) =
  Lex.CharCatToken $ Lex.CharCat (modChar char) cat
  where
    modChar = ord . switch dir . chr
    switch R.Upward = toUpper
    switch R.Downward = toLower
changeCase _ t = t

-- Things I can't easily parse outside this module, because of the recursive
-- parsing in ExpandedStream.
-- Constraining only the stream token type won't work easily either, because
-- the functions currently depend on the ExpandedStream type per se.
parseInhibited :: SimpLexParser a -> SimpExpandParser a
parseInhibited p = do
  P.State {stateInput = ExpandedStream (R.ResolvedStream lStream csMap)} <-
    P.getParserState
  case easyRunParser p lStream of
    (_, Left _) -> error "ohnoes"
    (P.State lStream' pos prc w, Right v) -> do
      P.setParserState
        (P.State (ExpandedStream $ R.ResolvedStream lStream' csMap) pos prc w)
      return v

parseGeneralText :: SimpParser ExpandedStream BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  -- TODO: Maybe other things can act as left braces.
  skipSatisfied isExplicitLeftBrace
  parseInhibited parseBalancedText
  where
    isFillerItem R.Relax = True
    isFillerItem t = isSpace t

parseCSNameArgs :: SimpParser ExpandedStream [CharCode]
parseCSNameArgs = do
  chars <- P.many $ satisfyThen tokToChar
  skipSatisfiedEquals (R.SyntaxCommandArg R.EndCSName)
  return chars
  where
    tokToChar (R.CharCat Lex.CharCat {char = c}) = Just c
    tokToChar _ = Nothing

parseChangeCaseArgs :: SimpParser ExpandedStream BalancedText
parseChangeCaseArgs = parseGeneralText

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
    let es' = ExpandedStream rs'
    case rt
      -- If it's a primitive token, provide that.
          of
      PrimitiveToken pt -> return (pt, es')
      -- If it indicates the start of a syntax command.
      SyntaxCommandHead (ChangeCaseToken direction)
        -- Parse the remainder of the syntax command.
       ->
        case easyRunParser parseChangeCaseArgs es' of
          (_, Left parseError) -> error $ show parseError
          (P.State es'' _ _ _, Right (BalancedText caseToks))
            -- Now perform take1_ on the stream after parsing, with the new
            -- tokens inserted.
           ->
            P.take1_ $ insertLexTokensE es'' $ changeCase direction <$> caseToks
      SyntaxCommandHead (MacroToken (Macro [] (BalancedText mToks))) ->
        P.take1_ $ insertLexTokensE es' mToks
      -- SyntaxCommandHead CSName ->
      --   case easyRunParser parseCSNameArgs es' of
      --     (_, Left parseError) -> error $ show parseError
      --     (P.State es' _ _ _, Right (charToks)) ->
      --       let csNameCSTok = Lex.ControlSequenceToken $ Lex.ControlSequence charToks
      --       in P.take1_ $ insertLexTokenE es' $ csNameCSTok

type SimpExpandParser = P.Parsec () ExpandedStream

insertLexTokenE :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexTokenE (ExpandedStream rs) t = ExpandedStream (insertLexTokenR rs t)

insertLexTokensE :: ExpandedStream -> [Lex.Token] -> ExpandedStream
insertLexTokensE (ExpandedStream rs) ts =
  ExpandedStream (insertLexTokensR rs ts)
