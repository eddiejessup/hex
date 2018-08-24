{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Util where

import Data.String.Utils (replace)
import qualified Text.Megaparsec as P
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Foldable (foldl')
import qualified Control.Monad.State.Lazy as MState
import Data.Functor (($>))

import qualified HeX.Expand as Expand
import HeX.Expand (ParseToken)
import qualified HeX.Lex as Lex
import qualified HeX.Categorise as Cat

type ParseTokens = [ParseToken]

type CharCodes = [Cat.CharCode]

data Stream = Stream { codes :: CharCodes
                     , lexTokens :: [Lex.Token]
                     , lexState :: Lex.LexState
                     , ccMap :: Cat.CharCatMap
                     , csMap :: Expand.CSMap
                     , expand :: Bool }

newStream :: [Cat.CharCode] -> Stream
newStream cs = Stream { codes=cs
                      , lexTokens=[]
                      , lexState=Lex.LineBegin
                      , ccMap=Cat.usableCharCatMap
                      , csMap=Expand.defaultCSMap
                      , expand=True }

insertLexToken :: Stream -> Lex.Token -> Stream
insertLexToken s t = s{lexTokens=t:lexTokens s}

insertLexTokens :: Stream -> [Lex.Token] -> Stream
-- TODO: This use of reverse is pure sloth; fix later.
insertLexTokens s ts = foldl' insertLexToken s $ reverse ts

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
  take1_ (Stream [] _ _ _ _ _) = Nothing
  -- If the lex token buffer is empty.
  take1_ stream@(Stream cs [] _lexState _ccMap _csMap _expand) = do
    (lexTok, _lexState', cs') <- Lex.extractToken getCC _lexState cs
    let parseTok = Expand.lexToParseToken _expand _csMap lexTok
    let stream' = stream{codes=cs', lexState=_lexState'}
    return (parseTok, stream')
    where
      getCC = Cat.extractCharCat (Cat.catLookup _ccMap)
  -- If there is a lex token in the buffer.
  take1_ stream@(Stream _ (lt:lts) _ _ _csMap _expand) = do
    let parseTok = Expand.lexToParseToken _expand _csMap lt
    let stream' = stream{lexTokens=lts}
    return (parseTok, stream')

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
skipOptional p = P.optional p $> ()

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
