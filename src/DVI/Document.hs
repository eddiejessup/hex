module DVI.Document where

import Safe (lastDef)
import qualified TFM

import DVI.Encode

-- Encode abstract instructions.
data Instruction
  = Character { charNr :: Int , move :: Bool }
  | Rule { height :: Int , width :: Int , move :: Bool }
  | BeginNewPage
  | MoveRight Int
  | MoveRightW
  | MoveRightX
  | MoveDown Int
  | MoveDownY
  | MoveDownZ
  | DefineFont { fontInfo :: TFM.TexFont
               , fontPath :: FilePath
               , fontNr :: Int
               , scaleFactorRatio :: Rational }
  | SelectFont Int
  | PushStack
  | PopStack
  | DoSpecial String
  deriving (Show)

-- History: (Encodable instructions, selected font number, begin-page pointers, stack depth, max stack depth)
parseMundaneInstructions :: [Instruction] -> Int -> Either String ([EncodableInstruction], Maybe Int, [Int], Int, Int)
parseMundaneInstructions [] magnification = Right ([getPreambleInstr magnification], Nothing, [], 0, 0)
parseMundaneInstructions (this:rest) magnification = do
  (restEncodeds, restCurrentFontNr, restBeginPagePointers, restStackDepth, restMaxStackDepth) <-
    parseMundaneInstructions rest magnification
  (thisEncodeds, thisCurrentFontNr, thisBeginPagePointers) <-
    case this of
      SelectFont _fontNr -> do
        instr <- getSelectFontNrInstruction _fontNr
        return ([instr], Just _fontNr, restBeginPagePointers)
      Character {charNr = _charNr, move = _move} -> do
        charInstr <- getCharacterInstruction _charNr _move
        return ([charInstr], restCurrentFontNr, restBeginPagePointers)
      Rule {width = w, height = h, move = _move} ->
        let _op = if _move then SetRule else PutRule
            args = [U4 $ fromIntegral h, U4 $ fromIntegral w]
            instr = EncodableInstruction _op args
        in return ([instr], restCurrentFontNr, restBeginPagePointers)
      BeginNewPage -> do
        let endRet =
              case restBeginPagePointers of
                [] -> []
                _ -> [endPageInstruction]
            beginPageInstr =
              getBeginPageInstruction $ lastDef (-1) restBeginPagePointers
            newBeginPagePointer = encLength (endRet ++ restEncodeds)
        fontRet <-
          case restCurrentFontNr of
            Just nr -> (:[]) <$> getSelectFontNrInstruction nr
            Nothing -> return []
        return
          ( fontRet ++ [beginPageInstr] ++ endRet
          , restCurrentFontNr
          , newBeginPagePointer : restBeginPagePointers)
      MoveRight dist -> do
        instr <- getMoveInstruction True dist
        return ([instr], restCurrentFontNr, restBeginPagePointers)
      MoveDown dist -> do
        instr <- getMoveInstruction False dist
        return ([instr], restCurrentFontNr, restBeginPagePointers)
      DefineFont { fontInfo = info
                 , fontPath = path
                 , fontNr = nr
                 , scaleFactorRatio = scaleRatio
                 } -> do
        let checksum = TFM.checksum info
            designSize = round $ TFM.designSizeSP info
            scaleFactor = TFM.designScaleSP info scaleRatio
        defineFontInstruction <-
          getDefineFontInstruction nr path scaleFactor designSize checksum
        return
          ([defineFontInstruction], restCurrentFontNr, restBeginPagePointers)
      PushStack ->
        return ([pushInstruction], restCurrentFontNr, restBeginPagePointers)
      PopStack ->
        return ([popInstruction], restCurrentFontNr, restBeginPagePointers)
  let thisStackDepth =
        restStackDepth +
        case this of
          PushStack -> 1
          PopStack -> -1
          _ -> 0
      thisMaxStackDepth = max thisStackDepth restMaxStackDepth
  return
    ( thisEncodeds ++ restEncodeds
    , thisCurrentFontNr
    , thisBeginPagePointers
    , thisStackDepth
    , thisMaxStackDepth)

parseInstructions :: [Instruction] -> Int -> Either String [EncodableInstruction]
parseInstructions instrs magnification = do
  (mundaneInstrs, _, beginPagePointers, _, maxStackDepth) <- parseMundaneInstructions (reverse instrs) magnification
  let (maxPageHeightPlusDepth, maxPageWidth) = (1, 1)
      postambleInstr =
        getPostambleInstr
          beginPagePointers
          magnification
          maxPageHeightPlusDepth
          maxPageWidth
          maxStackDepth
      finishedInstrs = endPageInstruction:mundaneInstrs
      postamblePointer = encLength finishedInstrs
      postPostambleInstr = getPostPostambleInstr postamblePointer
      fontDefinitions = filter isDefineFontInstr mundaneInstrs
  return $ [postPostambleInstr] ++
    fontDefinitions ++ [postambleInstr] ++ finishedInstrs
  where
    isDefineFontInstr (EncodableInstruction op _) = case op of
      Define1ByteFontNr -> True
      Define2ByteFontNr -> True
      Define3ByteFontNr -> True
      Define4ByteFontNr -> True
      _ -> False
