module DVI.Document where

import           HeXlude

import           Data.Char       (ord)
import qualified Data.Text       as Txt
import           Path            (Path)
import qualified Path

import           DVI.Encode      (encLength)
import           DVI.Instruction
import           DVI.Operation   (Operation (DefineFontNr))

data Rule = Rule { ruleWidth, ruleHeight, ruleDepth :: !LenVal }
    deriving ( Show )

instance Dimensioned Rule where
    naturalLength = \case
        BoxWidth  -> ruleWidth
        BoxHeight -> ruleHeight
        BoxDepth  -> ruleDepth

data Character =
    Character { char :: !Char, charWidth, charHeight, charDepth :: !LenVal }
    deriving ( Show )

instance Dimensioned Character where
    naturalLength = \case
        BoxWidth  -> charWidth
        BoxHeight -> charHeight
        BoxDepth  -> charDepth

instance Readable Character where
    describe v = "'" <> Txt.singleton (char v) <> "'"

data FontDefinition =
    FontDefinition { fontDefChecksum    :: Int
                   , fontDefDesignSize  :: Int
                   , fontDefDesignScale :: Int
                   , fontPath           :: !(Path Path.Rel Path.File)
                   , fontName           :: !Text
                   , fontNr             :: !Int
                   }
    deriving ( Show )

instance Readable FontDefinition where
    describe v = "Font at " <> show (fontPath v)

newtype FontSelection = FontSelection Int
    deriving ( Show )

instance Readable FontSelection where
    describe (FontSelection n) = "Select font " <> show n

data Instruction =
      AddCharacter !Character
    | AddRule !Rule
    | BeginNewPage
    | Move Axis !Int
    | DefineFont !FontDefinition
    | SelectFont !FontSelection
    | PushStack
    | PopStack
-- \| DoSpecial !Text
    deriving ( Show )

instance Readable Instruction where
    describe = \case
      AddCharacter c -> "Add " <> describe c
      AddRule r -> "Add Rule:" <> show r
      BeginNewPage -> "Begin Page"
      Move axis sp -> "Move " <> show axis <> " " <> showSP sp
      DefineFont v -> describe v
      SelectFont v -> describe v
      PushStack -> "Push stack"
      PopStack -> "Pop stack"

data ParseState =
    ParseState { instrs :: !(ForwardDirected Seq EncodableInstruction)
               , curFontNr :: !(Maybe Int)
               , beginPagePointers :: ![Int]
               , stackDepth :: !Int
               , maxStackDepth :: !Int
               }

initialParseState :: Int -> ParseState
initialParseState mag =
    ParseState { instrs = pure (getPreambleInstr mag)
               , curFontNr = Nothing
               , beginPagePointers = []
               , stackDepth = 0
               , maxStackDepth = 0
               }

addInstruction :: ParseState -> EncodableInstruction -> ParseState
addInstruction s@ParseState{ instrs } i = s{ instrs = instrs ->. i }

parseMundaneInstruction :: ParseState -> Instruction -> Either Text ParseState
parseMundaneInstruction st = \case
    SelectFont (FontSelection n) -> do
        st' <- addInstruction st <$> getSelectFontNrInstruction n
        pure st' { curFontNr = Just n }
    AddCharacter Character{char = c} ->
        addInstruction st <$> getCharacterInstruction (ord c) Set
    AddRule Rule{ruleWidth = w, ruleHeight = h, ruleDepth = d} ->
        pure $ addInstruction st $ getRuleInstruction Set (h + d) w
    Move ax dist -> addInstruction st <$> getMoveInstruction ax dist
    BeginNewPage -> do
        let points = beginPagePointers st
            -- Get the instructions to finish off the current page.
            -- If there are no begin-page pointers, we are on the first page,
            -- so don't add an end-page instruction.
            instrsEnded = case points of
                [] -> instrs st
                _  -> instrs st ->. endPageInstruction
            beginPageInstr = getBeginPageInstruction $ lastDef (-1) points
            instrsBegun = instrsEnded ->. beginPageInstr
        -- If a font is selected, add an instruction to select it again on the
        -- new page.
        instrsDone <- case curFontNr st of
            Just nr -> do
                fInstr <- getSelectFontNrInstruction nr
                pure (instrsBegun ->. fInstr)
            Nothing ->
                pure instrsBegun
        -- Update the instructions, and add a pointer for the new begin-page
        -- instruction.
        pure st { instrs = instrsDone
                , beginPagePointers = encLength instrsEnded : points
                }
    DefineFont FontDefinition{ fontDefChecksum
                             , fontDefDesignSize
                             , fontDefDesignScale
                             , fontPath = path
                             , fontNr = n
                             } ->
        addInstruction st <$> getDefineFontInstruction n path fontDefDesignScale fontDefDesignSize fontDefChecksum
    PushStack ->
        let st' = addInstruction st pushInstruction
            newDepth = succ $ stackDepth st'
        in
            pure st' { stackDepth    = newDepth
                     , maxStackDepth = max (maxStackDepth st') newDepth
                     }
    PopStack -> let st' = addInstruction st popInstruction
                in
                    pure st' { stackDepth = pred $ stackDepth st' }

parseMundaneInstructions :: Int -> ForwardDirected [] Instruction -> Either Text ParseState
parseMundaneInstructions mag _instrs = do
    st <- foldM parseMundaneInstruction (initialParseState mag) _instrs
    pure $ addInstruction st endPageInstruction

parseInstructions :: ForwardDirected [] Instruction -> Int -> Either Text (ForwardDirected Seq EncodableInstruction)
parseInstructions _instrs magnification = do
    ParseState{instrs = mundaneInstrs, beginPagePointers, maxStackDepth}
        <- parseMundaneInstructions magnification _instrs
    let (maxPageHeightPlusDepth, maxPageWidth) = (1, 1)
        postambleInstr =
            getPostambleInstr beginPagePointers
                              magnification
                              maxPageHeightPlusDepth
                              maxPageWidth
                              maxStackDepth
        postamblePointer = encLength mundaneInstrs
        postPostambleInstr = getPostPostambleInstr postamblePointer
        fontDefinitions = filter isDefineFontInstr mundaneInstrs
    pure $ (mundaneInstrs ->. postambleInstr) <> fontDefinitions ->. postPostambleInstr
  where
    isDefineFontInstr i = case i of
        EncodableInstruction (DefineFontNr _) _ -> True
        _                                       -> False
