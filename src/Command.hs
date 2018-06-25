{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Command where

import Data.String.Utils (replace)
import qualified Text.Megaparsec as P
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Char as C
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Harden
import Harden (ParseToken)
import qualified Lex
import qualified Cat

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show

data Distance = Distance Int
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
  -- TEMP: Dummy label constructor until properly implemented.
  = SelectFont
  -- | SelectFont FontDefToken
  -- | SetFamilyMember {member :: FamilyMember, font :: Font}
  -- | SetParShape
  -- | Read
  -- | DefineBox
  -- TEMP: Dummy label constructor until properly implemented.
  | DefineFont
  -- | DefineFont
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
  | EnterVMode
  -- | EnterMathMode
  -- | AddAdjustment VModeMaterial
  -- | AddControlSpace
  | AddCharacter { method :: CharSource, code :: Int }
  -- | AddAccentedCharacter { accentCode :: Int, targetCode :: Maybe Int, assignments :: [Assignment]}
  -- | AddItalicCorrection
  -- | AddDiscretionaryText { preBreak, postBreak, noBreak :: GeneralText }
  deriving Show

-- extractAllInner :: Stream -> [Command]
-- extractAllInner stream =
--   case P.parse hModeCommandParser "" stream of
--     Left _ -> []
--     Right (com, newStream) -> com:extractAllInner newStream

-- extractAllDebug :: Cat.CharCatMap -> [Cat.CharCode] -> [Command]
-- extractAllDebug ccMap cs = extractAllInner $ Stream{codes=cs, lexState=Lex.LineBegin, ccMap=ccMap}


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
  take1_ s@(Stream cs lexState0 _ccMap) = do
    (t, lexState1, rest) <- Harden.extractToken _ccMap lexState0 cs
    let s2 = s{codes=rest, lexState=lexState1}
    return (t, s2)

satisfy f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right x
        else Left (Just (P.Tokens (x:|[])), Set.empty)

atEnd :: Stream -> Bool
atEnd stream = case P.parse isEOF "" stream of
  (Right x) -> x
  (Left _) -> False
  where
    isEOF = P.atEnd :: P.Parsec () Stream Bool

-- HMode.

extractHModeCommand :: Stream -> Either (P.ParseError (P.Token Stream) ()) (HModeCommand, Stream)
extractHModeCommand stream = P.parse hModeCommandParser "" stream

hModeCommandParser :: P.Parsec () Stream (HModeCommand, Stream)
hModeCommandParser = do
  com <- P.choice hCommands
  P.State{stateInput=stream} <- P.getParserState
  return (com, stream)

hCommands =
  [ hRelax
  , hAddSpace
  , hAddCharacter
  , hStartParagraph
  , hEndParagraph
  , hTempDFont
  , hTempSFont
  ]

-- HMode Commands.

hRelax = do
  _ <- satisfy (== Harden.Relax)
  return $ HAllModesCommand Relax

hAddSpace = do
  _ <- satisfy (== Harden.Space)
  return $ HAllModesCommand AddSpace

hAddCharacter = do
  (Harden.ExplicitCharacter code) <- satisfy isExplicitCharacter
  return AddCharacter{method=ExplicitChar, code=code}

hStartParagraph = do
  (Harden.StartParagraph indent) <- satisfy isStartParagraph
  return $ HAllModesCommand StartParagraph{indent=indent}

hEndParagraph = do
  Harden.EndParagraph <- satisfy (== Harden.EndParagraph)
  return $ HAllModesCommand EndParagraph

hTempDFont = do
  _ <- satisfy (== Harden.MacroToFont)
  return $ HAllModesCommand $ Assign Assignment {body=DefineFont, global=False}

hTempSFont = do
  _ <- satisfy (== Harden.TokenForFont)
  return $ HAllModesCommand $ Assign Assignment {body=SelectFont , global=False}

-- VMode.

extractVModeCommand :: Stream -> Either (P.ParseError (P.Token Stream) ()) (VModeCommand, Stream)
extractVModeCommand stream = P.parse vModeCommandParser "" stream

vModeCommandParser :: P.Parsec () Stream (VModeCommand, Stream)
vModeCommandParser = do
  com <- P.choice vCommands
  P.State{stateInput=stream} <- P.getParserState
  return (com, stream)

vCommands =
  [ vRelax
  , vEndParagraph
  , vMacroToFont
  , vTokenForFont
  , vEnterHMode
  ]

-- VMode Commands.

vRelax = do
  _ <- satisfy (== Harden.Relax)
  return $ VAllModesCommand Relax

vEndParagraph = do
  _ <- satisfy (== Harden.EndParagraph)
  return $ VAllModesCommand EndParagraph

vMacroToFont = do
  _ <- satisfy (== Harden.MacroToFont)
  return $ VAllModesCommand $ Assign Assignment {body=DefineFont, global=False}

vTokenForFont = do
  _ <- satisfy (== Harden.TokenForFont)
  return $ VAllModesCommand $ Assign Assignment {body=SelectFont , global=False}

vEnterHMode = do
  _ <- satisfy startsHMode
  return EnterHMode

-- Token matching.

startsHMode (Harden.ExplicitCharacter _) = True
startsHMode _ = False
isExplicitCharacter (Harden.ExplicitCharacter _) = True
isExplicitCharacter _ = False
isStartParagraph (Harden.StartParagraph _) = True
isStartParagraph _ = False
