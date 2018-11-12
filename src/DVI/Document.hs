module DVI.Document where

import Control.Monad
import Safe (lastDef)

import TFM

import DVI.Encode
import DVI.Instruction

-- Encode abstract instructions.
data Instruction
  = Character Int MoveMode
  | Rule { height :: Int
         , width :: Int
         , move :: MoveMode }
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

data ParseState = ParseState { instrs :: [EncodableInstruction]
                             , curFontNr :: Maybe Int
                             , beginPagePointers :: [Int]
                             , stackDepth :: Int
                             , maxStackDepth :: Int }

initialParseState :: Int -> ParseState
initialParseState mag = ParseState { instrs=[getPreambleInstr mag]
                                   , curFontNr=Nothing
                                   , beginPagePointers=[]
                                   , stackDepth=0
                                   , maxStackDepth=0 }

addInstruction :: ParseState -> EncodableInstruction -> ParseState
addInstruction s@ParseState{instrs=_instrs} i = s{instrs=i:_instrs}

addInstruction' :: ParseState -> EncodableInstruction -> Either String ParseState
addInstruction' s i = return $ addInstruction s i

parseMundaneInstruction :: ParseState -> Instruction -> Either String ParseState
parseMundaneInstruction st@ParseState{instrs=_instrs} (SelectFont fNr') = do
  instr <- getSelectFontNrInstruction fNr'
  return st{instrs=instr:_instrs, curFontNr=Just fNr'}
parseMundaneInstruction st@ParseState{instrs=_instrs} (Character _charNr _move) =
  getCharacterInstruction _charNr _move >>= addInstruction' st
parseMundaneInstruction st@ParseState{instrs=_instrs} (Rule h w _move) =
  addInstruction' st $ getRuleInstruction _move h w
parseMundaneInstruction st@ParseState{instrs=_instrs} (MoveRight dist) =
  getMoveInstruction Horizontal dist >>= addInstruction' st
parseMundaneInstruction st@ParseState{instrs=_instrs} (MoveDown dist) =
  getMoveInstruction Vertical dist >>= addInstruction' st
parseMundaneInstruction st@ParseState{instrs=_instrs, beginPagePointers=points, curFontNr=fNr} BeginNewPage = do
  let beginPageInstr = getBeginPageInstruction $ lastDef (-1) points
      instrsEnded = case points of
        [] -> _instrs
        _ -> endPageInstruction : _instrs
  instrs' <- case fNr of
    Just nr -> do
      fInstr <- getSelectFontNrInstruction nr
      return $ fInstr : beginPageInstr : instrsEnded
    Nothing -> return $ beginPageInstr : instrsEnded
  return st{instrs=instrs', beginPagePointers=encLength instrsEnded : points}
parseMundaneInstruction st@ParseState{instrs=_instrs} DefineFont{fontInfo=info, fontPath=path, fontNr=defFontNr, scaleFactorRatio=scaleRatio} =
  let _checksum = checksum info
      designSize = round $ designSizeSP info
      scaleFactor = designScaleSP info scaleRatio
  in getDefineFontInstruction defFontNr path scaleFactor designSize _checksum >>= addInstruction' st
parseMundaneInstruction st@ParseState{instrs=_instrs, stackDepth=sDpth, maxStackDepth=mSDpth} PushStack =
  return st{instrs=pushInstruction:_instrs, stackDepth=succ sDpth, maxStackDepth=max mSDpth (succ sDpth)}
parseMundaneInstruction st@ParseState{instrs=_instrs, stackDepth=sDpth} PopStack =
  return st{instrs=popInstruction:_instrs, stackDepth=pred sDpth}

parseMundaneInstructions :: Int -> [Instruction] -> Either String ParseState
parseMundaneInstructions = foldM parseMundaneInstruction . initialParseState

parseInstructions :: [Instruction] -> Int -> Either String [EncodableInstruction]
parseInstructions _instrs magnification = do
  ParseState{instrs=mundaneInstrs, beginPagePointers=_beginPagePointers, maxStackDepth=_maxStackDepth} <- parseMundaneInstructions magnification _instrs
  let (maxPageHeightPlusDepth, maxPageWidth) = (1, 1)
      postambleInstr =
        getPostambleInstr
          _beginPagePointers
          magnification
          maxPageHeightPlusDepth
          maxPageWidth
          _maxStackDepth
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
