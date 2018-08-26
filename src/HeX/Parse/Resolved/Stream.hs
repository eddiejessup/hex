{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Resolved.Stream where

import qualified Data.HashMap.Strict as HMap
import Data.Maybe
import Data.Proxy
import qualified Text.Megaparsec as P

import HeX.Categorise (CharCode)
import qualified HeX.Lex as Lex

import HeX.Parse.Lexed.Stream

import HeX.Parse.Resolved.Token
import HeX.Parse.Resolved.Resolve

data ResolvedStream =
  ResolvedStream LexStream
                 CSMap

newResolvedStream :: [CharCode] -> CSMap -> ResolvedStream
newResolvedStream _ccMap = ResolvedStream (newLexStream _ccMap)


type ResolvedTokens = [ResolvedToken]

instance P.Stream ResolvedStream where
  type Token ResolvedStream = ResolvedToken
  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens ResolvedStream = ResolvedTokens
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
  take1_ (ResolvedStream s _csMap)
    -- Get the token and updated sub-stream.
   = do
    (lt, s') <- P.take1_ s
    -- Resolve the token.
    let t =
          case lt of
            (Lex.ControlSequence cs) ->
              let val = HMap.lookup (Lex.ControlSequenceProper cs) _csMap
                  err = error ("no such control sequence found: " ++ show cs)
              in fromMaybe err val
        -- TODO: Active characters.
            (Lex.CharCatToken cc) -> PrimitiveToken $ CharCat cc
    return (t, ResolvedStream s' _csMap)

type SimpResolveParser = P.Parsec () ResolvedStream

insertLexTokenR :: ResolvedStream -> Lex.Token -> ResolvedStream
insertLexTokenR (ResolvedStream ls csMap) t =
  ResolvedStream (insertLexToken ls t) csMap

insertLexTokensR :: ResolvedStream -> [Lex.Token] -> ResolvedStream
insertLexTokensR (ResolvedStream ls csMap) ts =
  ResolvedStream (insertLexTokens ls ts) csMap
