module DVI.Document where

import DVI.Encode (dviEncLength)
import DVI.Instruction
import DVI.Operation (Operation (DefineFontNr))
import Data.Byte (ByteError)
import Data.Path (PathError)
import qualified Data.Sequence as Seq
import Hex.Config.Codes
import Hex.Quantity
import Hexlude
import Path (Path)
import qualified Path

data Rule = Rule {ruleWidth, ruleHeight, ruleDepth :: Length}
  deriving stock Show

instance Dimensioned Rule where

  naturalLength = \case
    BoxWidth -> ruleWidth
    BoxHeight -> ruleHeight
    BoxDepth -> ruleDepth

data Character
  = Character {char :: CharCode, charWidth, charHeight, charDepth :: Length}
  deriving stock Show

instance Dimensioned Character where

  naturalLength = \case
    BoxWidth -> charWidth
    BoxHeight -> charHeight
    BoxDepth -> charDepth

instance Describe Character where

  describe Character {char} =
    [ (0, "Character")
    ] <> describeRel 1 char

data FontDefinition
  = FontDefinition
      { fontDefChecksum :: Int
      , fontDefDesignSize :: Length
      , fontDefDesignScale :: Length
      , fontPath :: Path Path.Rel Path.File
      , fontName :: Text
      , fontNr :: TeXInt
      }
  deriving stock Show

instance Describe FontDefinition where

  describe v =
    [ (0, "FontDefinition")
    , (1, "At" <> show (fontPath v))
    ]

newtype FontSelection = FontSelection TeXInt
  deriving stock Show

instance Describe FontSelection where

  describe (FontSelection n) =
    [ (0, "FontSelection")
    , (1, "Font number" <> show n)
    ]

data Instruction
  = AddCharacter !Character
  | AddRule !Rule
  | BeginNewPage
  | Move Axis !Length
  | DefineFont !FontDefinition
  | SelectFont !FontSelection
  | PushStack
  | PopStack
  -- \| DoSpecial !Text
  deriving stock Show

instance Describe Instruction where

  describe = \case
    AddCharacter c ->
      [ (0, "Instruction/AddCharacter")
      ]
      <> describeRel 1 c
    AddRule r ->
      [ (0, "Instruction/AddRule")
      , (1, show r)
      ]
    BeginNewPage ->
      [ (0, "Instruction/BeginNewPage")
      ]
    Move axis sp ->
      [ (0, "Instruction/Move")
      , (1, "Axis " <> quote (show axis))
      , (1, "By " <> quote (showSP sp))
      ]
    DefineFont v ->
      [ (0, "Instruction/DefineFont")
      ] <> describeRel 1 v
    SelectFont v ->
      [ (0, "Instruction/SelectFont")
      ] <> describeRel 1 v
    PushStack ->
      [ (0, "Instruction/PushStack")
      ]
    PopStack ->
      [ (0, "Instruction/PopStack")
      ]

data ParseState
  = ParseState
      { instrs :: Seq EncodableInstruction
      , curFontNr :: Maybe TeXInt
      , beginPagePointers :: [Int]
      , stackDepth :: Int
      , maxStackDepth :: Int
      }

initialParseState :: Int -> ParseState
initialParseState mag = ParseState
  { instrs = pure (getPreambleInstr mag)
  , curFontNr = Nothing
  , beginPagePointers = []
  , stackDepth = 0
  , maxStackDepth = 0
  }

addInstruction :: ParseState -> EncodableInstruction -> ParseState
addInstruction s@ParseState {instrs} i = s {instrs = instrs :|> i}

parseMundaneInstruction
  :: (MonadError e m, AsType ByteError e, AsType DVIError e, AsType PathError e)
  => ParseState
  -> Instruction
  -> m ParseState
parseMundaneInstruction st = \case
  SelectFont (FontSelection n) -> do
    st' <- addInstruction st <$> getSelectFontNrInstruction n
    pure st' {curFontNr = Just n}
  AddCharacter Character {char = c} ->
    addInstruction st <$> getCharacterInstruction c Set
  AddRule Rule {ruleWidth = w, ruleHeight = h, ruleDepth = d} ->
    pure $ addInstruction st $ getRuleInstruction Set (h + d) w
  Move ax dist ->
    addInstruction st <$> getMoveInstruction ax dist
  BeginNewPage -> do
    let points = beginPagePointers st
        -- Get the instructions to finish off the current page.
        -- If there are no begin-page pointers, we are on the first page,
        -- so don't add an end-page instruction.
        instrsEnded = case points of
          [] -> instrs st
          _ -> instrs st :|> endPageInstruction
        beginPageInstr = getBeginPageInstruction $ lastDef (-1) points
        instrsBegun = instrsEnded :|> beginPageInstr
    -- If a font is selected, add an instruction to select it again on the
    -- new page.
    instrsDone <-
      case curFontNr st of
        Just nr -> do
          fInstr <- getSelectFontNrInstruction nr
          pure (instrsBegun :|> fInstr)
        Nothing ->
          pure instrsBegun
    -- Update the instructions, and add a pointer for the new begin-page
    -- instruction.
    pure
      st
        { instrs = instrsDone
        , beginPagePointers = dviEncLength instrsEnded : points
        }
  DefineFont
    FontDefinition
      { fontDefChecksum
      , fontDefDesignSize
      , fontDefDesignScale
      , fontPath = path
      , fontNr = n
      } ->
      addInstruction st <$> getDefineFontInstruction n path fontDefDesignScale fontDefDesignSize fontDefChecksum
  PushStack ->
    let st' = addInstruction st pushInstruction
        newDepth = succ $ stackDepth st'
    in pure
         st'
           { stackDepth = newDepth
           , maxStackDepth = max (maxStackDepth st') newDepth
           }
  PopStack ->
    let st' = addInstruction st popInstruction
    in pure st' {stackDepth = pred $ stackDepth st'}

parseMundaneInstructions
  :: (MonadError e m, AsType ByteError e, AsType DVIError e, AsType PathError e)
  => Int
  -> Seq Instruction
  -> m ParseState
parseMundaneInstructions mag _instrs = do
  st <- foldM parseMundaneInstruction (initialParseState mag) _instrs
  pure $ addInstruction st endPageInstruction

parseInstructions
  :: (MonadError e m, AsType ByteError e, AsType DVIError e, AsType PathError e)
  => Seq Instruction
  -> Int
  -> m (Seq EncodableInstruction)
parseInstructions _instrs magnification = do
  ParseState {instrs = mundaneInstrs, beginPagePointers, maxStackDepth} <-
    parseMundaneInstructions magnification _instrs
  let (maxPageHeightPlusDepth, maxPageWidth) = (1, 1)
      postambleInstr =
        getPostambleInstr
          beginPagePointers
          magnification
          maxPageHeightPlusDepth
          maxPageWidth
          maxStackDepth
      postamblePointer = dviEncLength mundaneInstrs
      postPostambleInstr = getPostPostambleInstr postamblePointer
      fontDefinitions = Seq.filter isDefineFontInstr mundaneInstrs
  pure $ (mundaneInstrs :|> postambleInstr) <> fontDefinitions :|> postPostambleInstr
  where
    isDefineFontInstr i = case i of
      EncodableInstruction (DefineFontNr _) _ -> True
      _ -> False
