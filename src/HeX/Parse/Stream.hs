{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Stream where

import qualified Data.Char as C
import Data.Foldable (foldl')
import Data.Proxy
import Data.String.Utils (replace)
import qualified Text.Megaparsec as P

import qualified HeX.Categorise as Cat
import qualified HeX.Expand as Expand
import HeX.Expand (ParseToken)
import qualified HeX.Lex as Lex

type CharCodes = [Cat.CharCode]

type LexTokens = [Lex.Token]

data LexStream = LexStream
  { codes :: CharCodes
  , lexTokens :: LexTokens
  , lexState :: Lex.LexState
  , ccMap :: Cat.CharCatMap
  }

newLexStream :: CharCodes -> LexStream
newLexStream cs =
  LexStream
  { codes = cs
  , lexTokens = []
  , lexState = Lex.LineBegin
  , ccMap = Cat.usableCharCatMap
  }

insertLexToken :: LexStream -> Lex.Token -> LexStream
insertLexToken s t = s {lexTokens = t : lexTokens s}

insertLexTokens :: LexStream -> [Lex.Token] -> LexStream
-- TODO: This use of reverse is pure sloth; fix later.
insertLexTokens s ts = foldl' insertLexToken s $ reverse ts

showSrc :: String -> String
showSrc s = replace "\n" "\\n" (take 30 s)

instance Show LexStream where
  show LexStream {codes = cs, lexTokens = ts, lexState = ls} =
    show ls ++
    "; to-lex: " ++ show ts ++ "; \"" ++ showSrc (fmap C.chr cs) ++ "\""

instance P.Stream LexStream where
  type Token LexStream = Lex.Token
  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens LexStream = LexTokens
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
  take1_ (LexStream [] _ _ _) = Nothing
  -- If the lex token buffer is empty.
  take1_ stream@(LexStream cs [] _lexState _ccMap) = do
    (lt, _lexState', cs') <- Lex.extractToken getCC _lexState cs
    return (lt, stream {codes = cs', lexState = _lexState'})
    where
      getCC = Cat.extractCharCat (Cat.catLookup _ccMap)
  -- If there is a lex token in the buffer.
  take1_ stream@(LexStream _ (lt:lts) _ _) =
    return (lt, stream {lexTokens = lts})

type SimpLexParser = P.Parsec () LexStream

data ExpandedStream =
  ExpandedStream LexStream
                 Expand.CSMap

newExpandStream :: CharCodes -> Expand.CSMap -> ExpandedStream
newExpandStream _ccMap = ExpandedStream (newLexStream _ccMap)

type ParseTokens = [ParseToken]

instance P.Stream ExpandedStream where
  type Token ExpandedStream = ParseToken
  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens ExpandedStream = ParseTokens
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
  take1_ (ExpandedStream s _csMap)
    -- Get the token and updated sub-stream.
   = do
    (lt, s') <- P.take1_ s
    -- Expand the token, and return this updated stream.
    return (Expand.lexToParseToken _csMap lt, ExpandedStream s' _csMap)

type SimpExpandParser = P.Parsec () ExpandedStream

insertLexTokenE :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexTokenE (ExpandedStream ls csMap) t =
  ExpandedStream (insertLexToken ls t) csMap

insertLexTokensE :: ExpandedStream -> [Lex.Token] -> ExpandedStream
-- TODO: This use of reverse is pure sloth; fix later.
insertLexTokensE (ExpandedStream ls csMap) ts =
  ExpandedStream (insertLexTokens ls ts) csMap
