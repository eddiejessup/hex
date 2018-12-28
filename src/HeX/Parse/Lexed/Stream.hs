{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Lexed.Stream where

import           Data.Foldable                  ( foldl' )
import           Data.Proxy
import           Data.String.Utils              ( replace )
import qualified Text.Megaparsec               as P

import qualified HeX.Categorise                as Cat
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers

data LexStream = LexStream
  { codes :: [Cat.CharCode]
  , lexTokens :: [Lex.Token]
  , lexState :: Lex.LexState
  , ccMap :: Cat.CharCatMap }
  deriving Show

newLexStream :: [Cat.CharCode] -> LexStream
newLexStream cs = LexStream { codes = cs
                            , lexTokens = []
                            , lexState = Lex.LineBegin
                            , ccMap = Cat.usableCharCatMap }

insertLexToken :: LexStream -> Lex.Token -> LexStream
insertLexToken s t = s {lexTokens = t : lexTokens s}

insertLexTokens :: LexStream -> [Lex.Token] -> LexStream
-- TODO: This use of reverse is pure sloth; fix later.
insertLexTokens s ts = foldl' insertLexToken s $ reverse ts

showSrc :: String -> String
showSrc s = replace "\n" "\\n" (take 30 s)

showStream :: LexStream -> String
showStream LexStream { codes = cs, lexTokens = ts, lexState = ls }
  = show ls ++ "; to-lex: " ++ show ts ++ "; \"" ++ showSrc cs ++ "\""

instance P.Stream LexStream where
  type Token LexStream = Lex.Token

  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens LexStream = [Lex.Token]

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

  -- take1_ :: s -> Maybe (Token s, s)
  -- If we've no input, signal that we are done.
  take1_ (LexStream [] _ _ _) = Nothing
  -- If there is a lex token in the buffer, return that.
  take1_ stream@(LexStream _ (lt:lts) _ _) =
    pure (lt, stream {lexTokens = lts})
  -- If the lex token buffer is empty, extract a token and return it.
  take1_ stream@(LexStream cs [] _lexState _ccMap) = do
    (lt, _lexState', cs') <- Lex.extractToken getCC _lexState cs
    pure (lt, stream {codes = cs', lexState = _lexState'})
    where
      getCC = Cat.extractCharCat (Cat.catLookup _ccMap)

  takeN_ = undefined

  takeWhile_ = undefined

  showTokens Proxy = show

  reachOffset _ _freshState = (freshSourcePos, "", _freshState)

type SimpLexParser = SimpParser LexStream
