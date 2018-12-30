{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Resolved.Stream where

import           Data.Proxy
import           Data.Foldable                  ( foldl' )
import qualified Text.Megaparsec               as P

import qualified HeX.Categorise                as Cat
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Resolved.Token
import           HeX.Parse.Resolved.Resolve

data ResolvedStream = ResolvedStream { codes :: [Cat.CharCode]
                                     , lexTokens :: [Lex.Token]
                                     , lexState :: Lex.LexState
                                     , ccMap :: Cat.CharCatMap
                                     , csMap :: CSMap
                                     , expansionMode :: ExpansionMode }
  deriving (Show)

type SimpResolveParser = SimpParser ResolvedStream

newResolvedStream :: [Cat.CharCode] -> CSMap -> ResolvedStream
newResolvedStream cs _csMap = ResolvedStream { codes = cs
                                             , lexTokens = []
                                             , lexState = Lex.LineBegin
                                             , ccMap = Cat.usableCharCatMap
                                             , csMap = _csMap
                                             , expansionMode = Expanding }

insertLexTokenR :: ResolvedStream -> Lex.Token -> ResolvedStream
insertLexTokenR s t = s {lexTokens = t : lexTokens s}

-- TODO: This use of reverse is pure sloth; fix later.
insertLexTokensR :: ResolvedStream -> [Lex.Token] -> ResolvedStream
insertLexTokensR s ts = foldl' insertLexTokenR s $ reverse ts

setResStreamExpansion :: ExpansionMode -> ResolvedStream -> ResolvedStream
setResStreamExpansion m s = s { expansionMode = m }

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
  -- If we've no input, signal that we are done.
  take1_ ResolvedStream { codes = [] } = Nothing
  take1_ stream@(ResolvedStream cs lexBuff _lexState _ccMap _csMap expMode) =
    case lexBuff of
      -- If there is a lex token in the buffer, return that.
      (lt:lts) ->
        pure (res lt, stream { lexTokens = lts })
      -- If the lex token buffer is empty, extract a token and return it.
      [] -> do
        (lt, _lexState', cs') <- Lex.extractToken getCC _lexState cs
        pure (res lt, stream { codes = cs', lexState = _lexState' })
    where
      getCC = Cat.extractCharCat (Cat.catLookup _ccMap)
      res lexTok = resolveToken _csMap expMode lexTok

  takeN_ = undefined

  takeWhile_ = undefined

  showTokens Proxy = show

  reachOffset _ _freshState = (freshSourcePos, "", _freshState)
