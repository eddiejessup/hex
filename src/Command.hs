{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Command where

import Data.List.Split (chop)
import Data.String.Utils (replace)
import qualified Text.Megaparsec as P
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.Either.Combinators (rightToMaybe)

import qualified Harden
import Harden (ParseToken)
import qualified Lex
import qualified Cat

data Axis = Horizontal | Vertical
  deriving Show
data HDirection = Leftward | Rightward
  deriving Show
data VDirection = Upward | Downward
  deriving Show
data MessageStream = Out | Err
  deriving Show

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show
data SpaceSource = ExplicitSpace | ControlSpace
  deriving Show
data InsertionType = Insertion | Adjustment
  deriving Show

-- data Distance = Distance Int
  -- deriving Show
-- data HDisplacement = HDisplacement HDirection Distance | HDefault
  -- deriving Show
-- data VDisplacement = VDisplacement VDirection Distance | VDefault
  -- deriving Show
-- data BoxPlacement = HBox HDisplacement | VBox VDisplacement

-- TODO.
data MacroName = ActiveChar Char | ControlSequence String
  deriving Show
-- TODO.
data ParameterText = ParameterText [String]
  deriving Show
-- TODO.
data BalancedText = BalancedText [String]
  deriving Show


data Assignment
  = DefineMacro { name :: MacroName
                , parameters :: ParameterText
                , contents :: BalancedText
                , long :: Bool
                , outer :: Bool
                , expanded :: Bool }
  | SetVariable
  | ModifyVariable
  | AssignCode
  | Let
  | ShortDefine
  | SelectFont
  | SetFamilyMember
  | SetParShape
  | Read
  | DefineBox
  | DefineFont
  -- Global assignments.
  | SetFontAttribute
  | SetHyphenation
  | SetBoxSize
  | SetInteractionMode
  | SetSpecialVariable
  deriving Show

data Command
  = Relax
  | Assign {assignment :: Assignment, global :: Bool }
  -- | LeftBrace
  -- | RightBrace
  -- | BeginGroup
  -- | EndGroup
  -- | ShowToken
  -- | ShowBox
  -- | ShowLists
  -- | ShowInternalQuantity
  -- | ShipOut
  -- | IgnoreSpaces
  -- | SetAfterAssignmentToken
  -- | AddToAfterGroupTokens
  -- | Message MessageStream
  -- | OpenInput
  -- | CloseInput
  -- | OpenOutput
  -- | CloseOutput
  -- | Write
  -- | AddWhatsit
  -- | AddPenalty
  | AddKern Int
  -- | AddMathKern
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark
  -- | AddInsertion InsertionType
  -- | AddLeaders
  -- | AddBox BoxPlacement
  -- | UnpackBox Axis
  | EndParagraph { indent :: Bool }
  -- | EndParagraph
  -- | AddGlue Axis
  -- | AddRule Axis
  -- | AddAlignedMaterial Axis
  -- | End
  -- | Dump
  | AddSpace SpaceSource
  | AddCharacter { method :: CharSource, code :: Int }
  -- | AddAccent
  -- | AddItalicCorrection
  -- | AddDiscretionaryText
  -- | ShiftMathMode
  deriving Show

extractCommand :: Stream -> Maybe (Command, Stream)
extractCommand stream = rightToMaybe $ P.parse hParser "" stream

extractAllInner :: Stream -> [Command]
extractAllInner stream =
  case P.parse hParser "" stream of
    Left _ -> []
    Right (com, newStream) -> com:extractAllInner newStream

extractAllDebug :: Cat.CharCatMap -> [Cat.CharCode] -> [Command]
extractAllDebug ccMap cs = extractAllInner $ Stream{codes=cs, lexState=Lex.LineBegin, ccMap=ccMap}

instance Ord ParseToken where
  compare a b = EQ

instance Eq ParseToken where
  (==) a b = True

type ParseTokens = [ParseToken]

type CharCodes = [Cat.CharCode]

data Stream = Stream { codes :: CharCodes
                     , lexState :: Lex.LexState
                     , ccMap :: Cat.CharCatMap }

showSrc :: String -> String
showSrc s = replace "\n" "\\n" (take 30 s)

instance Show Stream where
  show Stream{codes=cs, lexState=ls} =
    (show ls) ++ "; \"" ++ (showSrc $ fmap C.chr cs) ++ "\""

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
  take1_ (Stream [] _ _) = Nothing
  take1_ s@(Stream cs lexState0 ccMap) = do
    (t, lexState1, rest) <- Harden.extractToken ccMap lexState0 cs
    let s2 = s{codes=rest, lexState=lexState1}
    return (t, s2)

satisfy f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right x
        else Left (Nothing, Set.empty)

hParser :: P.Parsec () Stream (Command, Stream)
hParser = do
  com <- P.choice commands
  P.State{stateInput=stream} <- P.getParserState
  return (com, stream)


isEOF :: P.Parsec () Stream Bool
isEOF = P.atEnd

atEnd :: Stream -> Bool
atEnd stream = case P.parse isEOF "" stream of
  (Right x) -> x
  (Left _) -> False

commands =
  [ relax
  , explicitSpace
  , explicitCharacter
  , endParagraph
  , tempDFont
  , tempSFont
  -- , kern (AddKern 2000000)
  ]

-- Commands.

relax = do
  _ <- satisfy isRelax
  return Relax

explicitSpace = do
  _ <- satisfy isSpace
  return $ AddSpace ExplicitSpace

explicitCharacter = do
  (Harden.ExplicitCharacter code) <- satisfy isExplicitCharacter
  return $ AddCharacter{method=ExplicitChar, code=code}

endParagraph = do
  (Harden.EndParagraph indent) <- satisfy isEndParagraph
  return $ EndParagraph{indent=indent}

tempDFont = do
  _ <- satisfy isTempDFont
  return $ Assign {assignment=DefineFont, global=False}

tempSFont = do
  _ <- satisfy isTempSFont
  return $ Assign {assignment=SelectFont, global=False}

-- Token matching.

isRelax Harden.Relax{} = True
isRelax _ = False
isSpace Harden.ExplicitSpace{} = True
isSpace _ = False
isExplicitCharacter Harden.ExplicitCharacter{} = True
isExplicitCharacter _ = False
isEndParagraph Harden.EndParagraph{} = True
isEndParagraph _ = False
isTempDFont Harden.TempDFont{} = True
isTempDFont _ = False
isTempSFont Harden.TempSFont{} = True
isTempSFont _ = False
