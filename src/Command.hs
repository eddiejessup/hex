{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Command where

import Data.String.Utils (replace)
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.List.NonEmpty (NonEmpty((:|)))
import Path (Path, Rel, File, parseRelFile)

import qualified Harden
import Harden (ParseToken)
import qualified Lex
import qualified Cat

import qualified Debug.Trace as T

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show

newtype Distance = Distance Int
  deriving Show

-- TODO.
data ControlSequenceLike = ActiveChar Char | ControlSequence String

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
  | DefineFont (Path Rel File) Int
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
  | Dump
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
                     , ccMap :: Cat.CharCatMap }

newStream :: [Cat.CharCode] -> Stream
newStream cs = Stream { codes=cs
                      , lexTokens=[]
                      , lexState=Lex.LineBegin
                      , ccMap=Cat.usableCharCatMap }

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
  take1_ (Stream [] _ _ _) = Nothing
  take1_ stream@(Stream cs [] _lexState _ccMap) = do
    (pt, lexStateNext, csNext) <- Harden.extractToken True _ccMap _lexState cs
    let streamNext = stream{codes=csNext, lexState=lexStateNext}
    return (pt, streamNext)
  take1_ stream@(Stream _ (lt:lts) _ _) =
    return (Harden.lexToParseToken True lt, stream{lexTokens=lts})

-- Helpers.

type Parser = P.Parsec () Stream
type ParseError = (P.ParseError (P.Token Stream) ())
type ParseState = P.State Stream

easyRunParser' :: Parser a -> Stream -> (ParseState, Either ParseError a)
easyRunParser' p stream =
  let
    pos = P.SourcePos "" (P.mkPos 0) (P.mkPos 0)
    state = P.State stream (pos:|[]) 0 (P.mkPos 0)
  in
    P.runParser' p state

satisfy :: (ParseToken -> Bool) -> Parser ParseToken
satisfy f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right x
        else Left (Just (P.Tokens (x:|[])), Set.empty)

-- All-mode Commands.

type AllModeCommandParser = Parser AllModesCommand

cRelax :: AllModeCommandParser
cRelax = do
  _ <- satisfy (== Harden.Relax)
  return Relax

cAddKern :: AllModeCommandParser
cAddKern = do
  _ <- satisfy (== Harden.AddKern)
  return $ AddKern (24 * (2^16))

cStartParagraph :: AllModeCommandParser
cStartParagraph = do
  (Harden.StartParagraph _indent) <- satisfy isStartParagraph
  return StartParagraph {indent=_indent}

cEndParagraph :: AllModeCommandParser
cEndParagraph = do
  _ <- satisfy (== Harden.EndParagraph)
  return EndParagraph

cMacroToFont :: AllModeCommandParser
cMacroToFont = do
  _ <- satisfy (== Harden.MacroToFont)
  fontNr <- parseFontNr
  fontPath <- parseFontPath
  return $ Assign Assignment {body=DefineFont fontPath fontNr, global=False}

cTokenForFont :: AllModeCommandParser
cTokenForFont = do
  (Harden.TokenForFont n) <- satisfy isTokenForFont
  return $ Assign Assignment {body=SelectFont n , global=False}

cAddSpace :: AllModeCommandParser
cAddSpace = do
  _ <- satisfy (== Harden.Space)
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

parseFontNr = do
  return Harden.theFontNr

parseFontPath = do
  let (Just theFontPath) = parseRelFile "support/cmr10.tfm"
  return theFontPath

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
  (Harden.ExplicitCharacter _code) <- satisfy isExplicitCharacter
  return AddCharacter{method=ExplicitChar, code=_code}

-- -- VMode.

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
  _ <- satisfy (== Harden.End)
  return End

vEnterHMode :: VModeCommandParser
vEnterHMode = do
  _ <- satisfy startsHMode
  return EnterHMode

-- Token matching.

type MatchToken = ParseToken -> Bool

endsHMode :: MatchToken
endsHMode Harden.End = True
endsHMode Harden.Dump = True
-- TODO:
-- - AddUnwrappedFetchedBox Vertical
-- - AddUnwrappedFetchedBox Vertical
-- - AddAlignedMaterial Horizontal
-- - AddRule Horizontal
-- - AddSpecifiedGlue Vertical
-- - AddPresetGlue Vertical
endsHMode _ = False

startsHMode :: MatchToken
startsHMode (Harden.ExplicitCharacter _) = True
-- TODO:
-- - Letter
-- - Other-character
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
startsHMode _ = False

isExplicitCharacter :: MatchToken
isExplicitCharacter (Harden.ExplicitCharacter _) = True
isExplicitCharacter _ = False

isTokenForFont :: MatchToken
isTokenForFont (Harden.TokenForFont _) = True
isTokenForFont _ = False

isStartParagraph :: MatchToken
isStartParagraph (Harden.StartParagraph _) = True
isStartParagraph _ = False
