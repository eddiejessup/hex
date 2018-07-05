{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Util where

import Data.String.Utils (replace)
import qualified Text.Megaparsec as P
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Control.Monad.State.Lazy as MState

import qualified Expand
import Expand (ParseToken)
import qualified Lex
import qualified Categorise as Cat

type ParseTokens = [ParseToken]

type CharCodes = [Cat.CharCode]

data Stream = Stream { codes :: CharCodes
                     , lexTokens :: [Lex.Token]
                     , lexState :: Lex.LexState
                     , ccMap :: Cat.CharCatMap
                     , expand :: Bool }

newStream :: [Cat.CharCode] -> Stream
newStream cs = Stream { codes=cs
                      , lexTokens=[]
                      , lexState=Lex.LineBegin
                      , ccMap=Cat.usableCharCatMap
                      , expand=True }

insertLexToken :: Stream -> Lex.Token -> Stream
insertLexToken s t = s{lexTokens=t:lexTokens s}

showSrc :: String -> String
showSrc s = replace "\n" "\\n" (take 30 s)

instance Show Stream where
  show Stream{codes=cs, lexTokens=ts, lexState=ls} =
    show ls ++ "; to-lex: " ++ show ts ++ "; \"" ++ showSrc (fmap C.chr cs) ++ "\""

instance P.Stream Stream where
  type Token Stream = ParseToken
  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens Stream = ParseTokens

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
  --
  take1_ (Stream [] _ _ _ _) = Nothing
  take1_ stream@(Stream cs [] _lexState _ccMap _expand) = do
    (pt, lexStateNext, csNext) <- Expand.extractToken _expand _ccMap _lexState cs
    let streamNext = stream{codes=csNext, lexState=lexStateNext}
    return (pt, streamNext)
  take1_ stream@(Stream _ (lt:lts) _ _ _expand) =
    return (Expand.lexToParseToken _expand lt, stream{lexTokens=lts})

-- Helpers.

data UserStateContent = UserStateContent
type UserState = MState.State UserStateContent
type Parser = P.ParsecT () Stream UserState
type ParseError = (P.ParseError (P.Token Stream) ())
type ParseState = P.State Stream
type MatchToken = ParseToken -> Bool
type NullParser = Parser ()

easyRunParser' :: Parser a -> Stream -> (ParseState, Either ParseError a)
easyRunParser' p stream =
  let
    pos = P.SourcePos "" (P.mkPos 1) (P.mkPos 1)
    parseState = P.State stream (pos:|[]) 0 (P.mkPos 1)
    (com, _) = MState.runState (P.runParserT' p parseState) UserStateContent
  in
    com

satisfy :: (ParseToken -> Bool) -> Parser ParseToken
satisfy f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right x
        else Left (Just (P.Tokens (x:|[])), Set.empty)

skipSatisfied :: (ParseToken -> Bool) -> NullParser
skipSatisfied f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right ()
        else Left (Just (P.Tokens (x:|[])), Set.empty)

skipSatisfiedEquals :: ParseToken -> NullParser
skipSatisfiedEquals t = skipSatisfied (== t)

satisfyThen :: (ParseToken -> Maybe a) -> Parser a
satisfyThen f = P.token testTok Nothing
  where
    testTok x =
      case f x of
        Just y -> Right y
        Nothing -> Left (Just (P.Tokens (x:|[])), Set.empty)

skipOptional :: Parser a -> NullParser
skipOptional p = do
  _ <- P.optional p
  return ()

skipOneOptionalSatisfied :: (ParseToken -> Bool) -> NullParser
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: (ParseToken -> Bool) -> NullParser
skipManySatisfied = P.skipMany . skipSatisfied

setExpansion :: Bool -> Stream -> Stream
setExpansion e s = s{expand=e}

setExpansionState :: Bool -> ParseState -> ParseState
setExpansionState e st@P.State{stateInput=stream} = st{P.stateInput=setExpansion e stream}

disableExpansion :: NullParser
disableExpansion = P.updateParserState $ setExpansionState False
enableExpansion :: NullParser
enableExpansion = P.updateParserState $ setExpansionState True
