{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Resolved.Stream where

import           Data.Proxy
import qualified Text.Megaparsec               as P

import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Lexed.Stream
import           HeX.Parse.Resolved.Token
import           HeX.Parse.Resolved.Resolve

data ResolvedStream = ResolvedStream { lexStream :: LexStream
                                     , csMap :: CSMap
                                     , expansionMode :: ExpansionMode }
  deriving (Show)

type SimpResolveParser = SimpParser ResolvedStream

newResolvedStream :: [CharCode] -> CSMap -> ResolvedStream
newResolvedStream cs _csMap = ResolvedStream (newLexStream cs) _csMap Expanding

insertLexTokenR :: ResolvedStream -> Lex.Token -> ResolvedStream
insertLexTokenR s t = insertLexTokensR s [t]

insertLexTokensR :: ResolvedStream -> [Lex.Token] -> ResolvedStream
insertLexTokensR (ResolvedStream _lexState _csMap expMode) lexToks
  = ResolvedStream (insertLexTokens _lexState lexToks) _csMap expMode

setResStreamExpansion :: ExpansionMode -> ResolvedStream -> ResolvedStream
setResStreamExpansion m s = s{expansionMode=m}

instance P.Stream ResolvedStream where
  type Token ResolvedStream = ResolvedToken

  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens ResolvedStream = [ResolvedToken]

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
  take1_ (ResolvedStream s _csMap expMode) = do
    -- Get the input token and updated sub-stream.
    (lexTok, s') <- P.take1_ s
    pure (resolveToken _csMap expMode lexTok, ResolvedStream s' _csMap expMode)

  takeN_ = undefined

  takeWhile_ = undefined

  showTokens Proxy = show

  reachOffset _ _freshState = (freshSourcePos, "", _freshState)
