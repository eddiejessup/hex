module DVI.Document where

import Control.Monad
import Safe (lastDef)
import qualified TFM
import TFM.Parse (TexFont, checksum)

import DVI.Encode

-- Encode abstract instructions.
data Instruction
  = Character { charNr :: Int
              , move :: Bool }
  | Rule { height :: Int
         , width :: Int
         , move :: Bool }
  | BeginNewPage
  | MoveRight Int
  | MoveRightW
  | MoveRightX
  | MoveDown Int
  | MoveDownY
  | MoveDownZ
  | DefineFont { fontInfo :: TexFont
               , fontPath :: FilePath
               , fontNr :: Int
               , scaleFactorRatio :: Rational }
  | SelectFont Int
  | PushStack
  | PopStack
  | DoSpecial String
  deriving (Show)

-- History: (Encodable instructions, selected font number, begin-page pointers, stack depth, max stack depth)
type ParseState = ([EncodableInstruction], Maybe Int, [Int], Int, Int)

parseMundaneInstruction :: ParseState -> Instruction -> Either String ParseState
parseMundaneInstruction (acc, fNr, points, sDep, maxSDep) this =
  case this of
    SelectFont fNr' -> do
      instr <- getSelectFontNrInstruction fNr'
      return (instr : acc, Just fNr', points, sDep, maxSDep)
    Character {charNr = _charNr, move = _move} ->
      getCharacterInstruction _charNr _move >>= addInstr
    Rule {width = w, height = h, move = _move} ->
      let op =
            (if _move
               then SetRule
               else PutRule)
          args = fmap (U4 . fromIntegral) [h, w]
       in addInstr $ EncodableInstruction op args
    BeginNewPage ->
      let beginPageInstr = getBeginPageInstruction $ lastDef (-1) points
          accEnded =
            case points of
              [] -> acc
              _ -> endPageInstruction : acc
          newBeginPagePointer = encLength accEnded
       in do acc' <-
               case fNr of
                 Just nr -> do
                   fInstr <- getSelectFontNrInstruction nr
                   return $ fInstr : beginPageInstr : accEnded
                 Nothing -> return $ beginPageInstr : accEnded
             return (acc', fNr, newBeginPagePointer : points, sDep, maxSDep)
    MoveRight dist -> getMoveInstruction True dist >>= addInstr
    MoveDown dist -> getMoveInstruction False dist >>= addInstr
    DefineFont { fontInfo = info
               , fontPath = path
               , fontNr = defFontNr
               , scaleFactorRatio = scaleRatio
               } ->
      let _checksum = checksum info
          designSize = round $ TFM.designSizeSP info
          scaleFactor = TFM.designScaleSP info scaleRatio
          instr =
            getDefineFontInstruction
              defFontNr
              path
              scaleFactor
              designSize
              _checksum
       in instr >>= addInstr
    PushStack ->
      return
        (pushInstruction : acc, fNr, points, sDep + 1, max maxSDep (sDep + 1))
    PopStack -> return (popInstruction : acc, fNr, points, sDep - 1, maxSDep)
  where
    addInstr i = return (i : acc, fNr, points, sDep, maxSDep)

parseMundaneInstructions :: Int -> [Instruction] -> Either String ParseState
parseMundaneInstructions mag =
  foldM parseMundaneInstruction ([getPreambleInstr mag], Nothing, [], 0, 0)

parseInstructions ::
     [Instruction] -> Int -> Either String [EncodableInstruction]
parseInstructions instrs magnification = do
  (mundaneInstrs, _, beginPagePointers, _, maxStackDepth) <-
    parseMundaneInstructions magnification instrs
  let (maxPageHeightPlusDepth, maxPageWidth) = (1, 1)
      postambleInstr =
        getPostambleInstr
          beginPagePointers
          magnification
          maxPageHeightPlusDepth
          maxPageWidth
          maxStackDepth
      finishedInstrs = endPageInstruction : mundaneInstrs
      postamblePointer = encLength finishedInstrs
      postPostambleInstr = getPostPostambleInstr postamblePointer
      fontDefinitions = filter isDefineFontInstr mundaneInstrs
  return $
    postPostambleInstr : fontDefinitions ++ postambleInstr : finishedInstrs
  where
    isDefineFontInstr (EncodableInstruction op _) =
      case op of
        Define1ByteFontNr -> True
        Define2ByteFontNr -> True
        Define3ByteFontNr -> True
        Define4ByteFontNr -> True
        _ -> False
