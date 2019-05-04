module DVI.Document where

import           HeXlude

import           Data.Char       ( ord )
import           Path            ( Path )
import qualified Path            as Path

import           DVI.Encode      ( encLength )
import           DVI.Instruction
import           DVI.Operation   ( Operation(DefineFontNr) )

import qualified TFM

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

data FontDefinition =
    FontDefinition { fontInfo         :: !TFM.TexFont
                   , fontPath         :: !(Path Path.Rel Path.File)
                   , fontName         :: !Text
                   , fontNr           :: !Int
                   , scaleFactorRatio :: !Rational
                   }
    deriving ( Show )

newtype FontSelection = FontSelection Int
    deriving ( Show )

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

data ParseState =
    ParseState { instrs :: ![EncodableInstruction]
               , curFontNr :: !(Maybe Int)
               , beginPagePointers :: ![Int]
               , stackDepth :: !Int
               , maxStackDepth :: !Int
               }

initialParseState :: Int -> ParseState
initialParseState mag =
    ParseState { instrs = [ getPreambleInstr mag ]
               , curFontNr = Nothing
               , beginPagePointers = []
               , stackDepth = 0
               , maxStackDepth = 0
               }

addInstruction :: ParseState -> EncodableInstruction -> ParseState
addInstruction s@ParseState{instrs = _instrs} i = s { instrs = i : _instrs }

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
                _  -> endPageInstruction : instrs st
            beginPageInstr = getBeginPageInstruction $ lastDef (-1) points
            instrsBegun = beginPageInstr : instrsEnded
        -- If a font is selected, add an instruction to select it again on the
        -- new page.
        instrsDone <- case curFontNr st of
            Just nr -> do
                fInstr <- getSelectFontNrInstruction nr
                pure $ fInstr : instrsBegun
            Nothing -> pure $ instrsBegun
        -- Update the instructions, and add a pointer for the new begin-page
        -- instruction.
        pure st { instrs = instrsDone
                , beginPagePointers = encLength instrsEnded : points
                }
    DefineFont FontDefinition{ fontInfo = info
                             , fontPath = path
                             , fontNr = n
                             , scaleFactorRatio = scaleRatio
                             } ->
        let cs = TFM.checksum info
            ds = round $ TFM.designSizeSP info
            sf = TFM.designScaleSP info scaleRatio
        in
            addInstruction st <$> getDefineFontInstruction n path sf ds cs
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

parseMundaneInstructions :: Int -> [Instruction] -> Either Text ParseState
parseMundaneInstructions mag _instrs = do
    st <- foldM parseMundaneInstruction (initialParseState mag) _instrs
    pure $ addInstruction st endPageInstruction

parseInstructions :: [Instruction] -> Int -> Either Text [EncodableInstruction]
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
    pure $
        postPostambleInstr
        : (fontDefinitions <> (postambleInstr : mundaneInstrs))
  where
    isDefineFontInstr i = case i of
        EncodableInstruction (DefineFontNr _) _ -> True
        _ -> False
