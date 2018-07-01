{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse where

import Data.String.Utils (replace)
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.List.NonEmpty (NonEmpty((:|)))
import Path (Path, Rel, File, parseRelFile)
import qualified Control.Monad.State.Lazy as MState

import qualified Expand
import Expand (ParseToken)
import qualified Lex
import qualified Categorise as Cat

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show

newtype Distance = Distance Int
  deriving Show

-- TODO.
data ControlSequenceLike = ActiveCharacter Cat.CharCode | ControlSequence Lex.ControlSequence
  deriving Show

makeCW :: String -> ControlSequenceLike
makeCW s = ControlSequence $ Lex.ControlWord s

data AssignmentBody
  -- = DefineMacro { name :: ControlSequenceLike
  --               , parameters :: ParameterText
  --               , contents :: BalancedText
  --               , long, outer, expanded :: Bool }
  -- | ShortDefine {quantity :: QuantityType, name :: ControlSequenceLike, value :: Int}
  -- | SetVariable VariableAssignment
  -- | ModifyVariable VariableModification
  -- | AssignCode { codeType :: CodeType, codeIndex, value :: Int }
  -- | Let { future :: Bool, name :: ControlSequenceLike, target :: Token}
  -- | FutureLet { name :: ControlSequenceLike, token1, token2 :: Token}
  = SelectFont Int
  -- | SetFamilyMember {member :: FamilyMember, font :: Font}
  -- | SetParShape
  -- | Read
  -- | DefineBox
  -- TEMP: Dummy label constructor until properly implemented.
  | DefineFont ControlSequenceLike (Path Rel File)
  -- -- Global assignments.
  -- | SetFontAttribute
  -- | SetHyphenation
  -- | SetBoxSize
  -- | SetInteractionMode
  -- | SetSpecialVariable
  deriving Show

data Assignment
  = Assignment { body :: AssignmentBody, global :: Bool }
  deriving Show

data AllModesCommand
  = Relax
  | Assign Assignment
  -- | LeftBrace
  -- | RightBrace
  -- | BeginGroup
  -- | EndGroup
  -- | ShowToken Token
  -- | ShowBox Int
  -- | ShowLists
  -- | ShowInternalQuantity InternalQuantity
  -- | ShipOut Box
  -- | IgnoreSpaces
  -- | SetAfterAssignmentToken Token
  -- | AddToAfterGroupTokens Tokens
  -- | ChangeCase VDirection GeneralText
  -- | Message MessageStream GeneralText
  -- | OpenInput { streamNr :: Int, fileName :: String }
  -- | CloseInput { streamNr :: Int }
  -- | OpenOutput { streamNr :: Int, fileName :: String, immediate :: Bool }
  -- | CloseOutput { streamNr :: Int, immediate :: Bool }
  -- | Write { streamNr :: Int, contents :: GeneralText, immediate :: Bool }
  -- | AddWhatsit GeneralText
  -- | AddPenalty Int
  | AddKern Int
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark GeneralText
  -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
  -- -- then can 'migrate' out.
  -- | AddInsertion {nr :: Int, contents :: VModeMaterial}
  -- | AddGlue Glue
  -- | AddLeaders {type :: LeadersType, template :: BoxOrRule, glue :: Glue}
  | AddSpace
  -- | AddBox Box
  -- | AddShiftedBox Distance Box
  -- | AddFetchedBox { register :: Int, unwrap, pop :: Bool } -- \box, \copy, \un{v,h}{box,copy}
  -- | AddRule { width, height, depth :: Maybe Distance }
  -- | AddAlignedMaterial DesiredLength AlignmentMaterial
  | StartParagraph { indent :: Bool }
  | EndParagraph
  deriving Show

data VModeCommand
  = VAllModesCommand AllModesCommand
  | EnterHMode
  | End
  -- | Dump
  deriving Show

data HModeCommand
  = HAllModesCommand AllModesCommand
  | LeaveHMode
  -- | EnterMathMode
  -- | AddAdjustment VModeMaterial
  -- | AddControlSpace
  | AddCharacter { method :: CharSource, code :: Int }
  -- | AddAccentedCharacter { accentCode :: Int, targetCode :: Maybe Int, assignments :: [Assignment]}
  -- | AddItalicCorrection
  -- | AddDiscretionaryText { preBreak, postBreak, noBreak :: GeneralText }
  deriving Show

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

skipOptional p = do
  _ <- P.optional p
  return ()

-- All-mode Commands.

type AllModeCommandParser = Parser AllModesCommand

cRelax :: AllModeCommandParser
cRelax = do
  _ <- satisfy (== Expand.Relax)
  return Relax

cAddKern :: AllModeCommandParser
cAddKern = do
  _ <- satisfy (== Expand.AddKern)
  return $ AddKern (24 * (2^16))

cStartParagraph :: AllModeCommandParser
cStartParagraph = do
  (Expand.StartParagraph _indent) <- satisfy isStartParagraph
  return StartParagraph {indent=_indent}

cEndParagraph :: AllModeCommandParser
cEndParagraph = do
  _ <- satisfy (== Expand.EndParagraph)
  return EndParagraph

-- \font <control-sequence> <equals> <file-name> <at-clause>
cMacroToFont :: AllModeCommandParser
cMacroToFont = do
  _ <- satisfy (== Expand.MacroToFont)
  cs <- parseCSName
  skipOptionalEquals
  fontPath <- parseFileName
  return $ Assign Assignment {body=DefineFont cs fontPath, global=False}

cTokenForFont :: AllModeCommandParser
cTokenForFont = do
  (Expand.TokenForFont n) <- satisfy isTokenForFont
  return $ Assign Assignment {body=SelectFont n , global=False}

cAddSpace :: AllModeCommandParser
cAddSpace = do
  _ <- satisfy isSpace
  return AddSpace

cCommands :: [Parser AllModesCommand]
cCommands =
  [ cRelax
  , cAddKern
  , cStartParagraph
  , cEndParagraph
  , cMacroToFont
  , cTokenForFont
  , cAddSpace
  ]

parseAllModeCommand :: Parser AllModesCommand
parseAllModeCommand = P.choice cCommands

suppressExpansion :: Bool -> Stream -> Stream
suppressExpansion e s = s{expand=e}

setExpansionState :: Bool -> ParseState -> ParseState
setExpansionState e st@P.State{stateInput=stream} = st{P.stateInput=suppressExpansion e stream}

disableExpansion :: Parser ()
disableExpansion = P.updateParserState $ setExpansionState False
enableExpansion :: Parser ()
enableExpansion = P.updateParserState $ setExpansionState True

parseCSName :: Parser ControlSequenceLike
parseCSName = do
  disableExpansion
  csLike <- parseCSLikeCS <|> parseCSLikeActive
  enableExpansion
  return csLike
  where
    parseCSLikeActive = do
      (Expand.CharCat Lex.LexCharCat{char=_code}) <- satisfy isActiveCharacter
      return $ ActiveCharacter _code
    parseCSLikeCS = do
      (Expand.UnexpandedControlSequence cs) <- satisfy isUnexpandedControlSequence
      return $ ControlSequence cs

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName = do
  skipOptionalSpaces
  nameToks <- P.some $ satisfy isLetterOrDigit
  let name = fmap (\(Expand.CharCat Lex.LexCharCat{char=c}) -> C.chr c) nameToks
  _ <- satisfy isSpace
  case parseRelFile (name ++ ".tfm") of
    Just p -> return p
    Nothing -> fail $ "Invalid filename: " ++ name ++ ".tfm"

skipOptionalEquals = do
  skipOptionalSpaces
  skipOptional $ satisfy isEquals

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces = P.skipMany $ satisfy isSpace


-- HMode.

type HModeCommandParser = Parser HModeCommand

extractHModeCommand :: Stream -> (ParseState, Either ParseError HModeCommand)
extractHModeCommand = easyRunParser' parseHModeCommand

parseHModeCommand :: Parser HModeCommand
parseHModeCommand =
  P.choice hCommands
  <|>
  (HAllModesCommand <$> parseAllModeCommand)

hCommands :: [HModeCommandParser]
hCommands =
  [ hLeaveHMode
  , hAddCharacter
  ]

-- HMode Commands.

hLeaveHMode :: HModeCommandParser
hLeaveHMode = do
  _ <- satisfy endsHMode
  return LeaveHMode

hAddCharacter :: HModeCommandParser
hAddCharacter = do
  (Expand.CharCat Lex.LexCharCat{char=_code}) <- satisfy isLetterOrOther
  return AddCharacter{method=ExplicitChar, code=_code}

-- VMode.

type VModeCommandParser = Parser VModeCommand

extractVModeCommand :: Stream -> (ParseState, Either ParseError VModeCommand)
extractVModeCommand = easyRunParser' parseVModeCommand

parseVModeCommand :: Parser VModeCommand
parseVModeCommand =
  P.choice vCommands
  <|>
  (VAllModesCommand <$> parseAllModeCommand)

vCommands :: [VModeCommandParser]
vCommands =
  [ vEnterHMode
  , vEnd
  ]

-- VMode Commands.

vEnd :: VModeCommandParser
vEnd = do
  _ <- satisfy (== Expand.End)
  return End

vEnterHMode :: VModeCommandParser
vEnterHMode = do
  _ <- satisfy startsHMode
  return EnterHMode

-- Token matching.

type MatchToken = ParseToken -> Bool

endsHMode :: MatchToken
endsHMode Expand.End = True
-- endsHMode Expand.Dump = True
-- TODO:
-- - AddUnwrappedFetchedBox Vertical
-- - AddUnwrappedFetchedBox Vertical
-- - AddAlignedMaterial Horizontal
-- - AddRule Horizontal
-- - AddSpecifiedGlue Vertical
-- - AddPresetGlue Vertical
endsHMode _ = False

startsHMode :: MatchToken
startsHMode x
  | isLetterOrOther x = True
  | otherwise = False
-- TODO:
-- - \char
-- - TokenForCharacter
-- - AddUnwrappedFetchedBox Horizontal
-- - AddUnwrappedFetchedBox Horizontal
-- - AddAlignedMaterial Vertical
-- - AddRule Vertical
-- - AddSpecifiedGlue Horizontal
-- - AddPresetGlue Horizontal
-- - AddAccentedCharacter
-- - AddItalicCorrection
-- - AddDiscretionaryText
-- - AddDiscretionaryHyphen
-- - ToggleMathMode

isLetterOrOther :: MatchToken
isLetterOrOther x = isLetter x || isOther x

isTokenForFont :: MatchToken
isTokenForFont (Expand.TokenForFont _) = True
isTokenForFont _ = False

isStartParagraph :: MatchToken
isStartParagraph (Expand.StartParagraph _) = True
isStartParagraph _ = False

isUnexpandedControlSequence :: MatchToken
isUnexpandedControlSequence (Expand.UnexpandedControlSequence _) = True
isUnexpandedControlSequence _ = False

isActiveCharacter :: MatchToken
isActiveCharacter (Expand.CharCat Lex.LexCharCat{cat=Lex.Active}) = True
isActiveCharacter _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: MatchToken
isSpace (Expand.CharCat Lex.LexCharCat{cat=Lex.Space}) = True
isSpace _ = False

isEquals :: MatchToken
isEquals (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=61}) = True
isEquals _ = False

isLetterOrDigit :: MatchToken
isLetterOrDigit x = isLetter x || isDigit x

isLetter :: MatchToken
isLetter (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter}) = True
isLetter _ = False

isOther :: MatchToken
isOther (Expand.CharCat Lex.LexCharCat{cat=Lex.Other}) = True
isOther _ = False

isDigit :: MatchToken
isDigit x = isOctalDigit x || isEightOrNine x
  where
    -- '8' or '9'.
    isEightOrNine (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=56}) = True
    isEightOrNine (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=57}) = True
    isEightOrNine _ = False

isOctalDigit :: MatchToken
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=48}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=49}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=50}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=51}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=52}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=53}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=54}) = True
isOctalDigit (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=55}) = True
isOctalDigit _ = False
