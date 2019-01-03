module DVI.Instruction where

import           Safe                           ( lastDef )
import           System.FilePath                ( splitFileName )

import           Data.Concept
import           Data.Byte
import           DVI.Encode

pickOp :: (Operation, Operation, Operation, Operation) -> IntArgVal -> Operation
pickOp (o1, o2, _, o4) v = case v of
  (S1 _) -> o1
  (U1 _) -> o1
  (S2 _) -> o2
  (U2 _) -> o2
  (S4 _) -> o4
  (U4 _) -> o4

getVariByteOpAndArg
  :: (Operation, Operation, Operation, Operation)
  -> SignableInt
  -> Either String (Operation, ArgVal)
getVariByteOpAndArg ops sI = do
  iArgVal <- intArgValFromSignableInt sI
  pure (pickOp ops iArgVal, IntArgVal iArgVal)

getVariByteInstruction
  :: (Operation, Operation, Operation, Operation)
  -> SignableInt
  -> Either String EncodableInstruction
getVariByteInstruction ops sI = do
  (op, arg) <- getVariByteOpAndArg ops sI
  pure $ EncodableInstruction op [arg]

getSelectFontNrInstruction :: Int -> Either String EncodableInstruction
getSelectFontNrInstruction fNr
  | fNr < 64 =
    pure $ EncodableInstruction (toEnum $ fNr + fromEnum SelectFontNr0) []
  | otherwise = do
    sI <- toUnsignedInt fNr
    getVariByteInstruction (Select1ByteFontNr, Select2ByteFontNr, Select3ByteFontNr, Select4ByteFontNr) sI

getSimpleEncInstruction :: Operation -> EncodableInstruction
getSimpleEncInstruction _op = EncodableInstruction _op []

endPageInstruction, pushInstruction, popInstruction :: EncodableInstruction
endPageInstruction = getSimpleEncInstruction EndPage
pushInstruction = getSimpleEncInstruction Push
popInstruction = getSimpleEncInstruction Pop

getBeginPageInstruction :: Int -> EncodableInstruction
getBeginPageInstruction lastBeginPoint =
  let boringArgs = fmap (IntArgVal . S4 . fromIntegral) ([0 .. 9] :: [Int])
      lastBeginPointArg = (IntArgVal . S4 . fromIntegral) lastBeginPoint
      args = boringArgs ++ [lastBeginPointArg]
  in EncodableInstruction BeginPage args

getDefineFontInstruction
  :: Int
  -> FilePath
  -> Int
  -> Int
  -> Int
  -> Either String EncodableInstruction
getDefineFontInstruction fNr fPath scaleFactor designSize fontChecksum = do
  sI <- toUnsignedInt fNr
  (_op, fontNrArgVal) <- getVariByteOpAndArg (Define1ByteFontNr, Define2ByteFontNr, Define3ByteFontNr, Define4ByteFontNr) sI
  let (dirPathRaw, fileName) = splitFileName fPath
      dirPath =
        if dirPathRaw == "./"
          then ""
          else dirPathRaw
      args =
        [ fontNrArgVal -- font_nr
        , (IntArgVal . U4 . fromIntegral) fontChecksum -- checksum
        , (IntArgVal . S4 . fromIntegral) scaleFactor -- scale_factor
        , (IntArgVal . S4 . fromIntegral) designSize -- design_size
        , (IntArgVal . U1 . fromIntegral . length) dirPath -- dir_path_length
        , (IntArgVal . U1 . fromIntegral . length) fileName -- file_name_length
        , StringArgVal fPath -- font_path
        ]
  pure $ EncodableInstruction _op args

getCharacterInstruction :: Int -> MoveMode -> Either String EncodableInstruction
getCharacterInstruction code Set
  | code < 128 = pure $ EncodableInstruction (toEnum $ code + fromEnum SetChar0) []
getCharacterInstruction code mode
  = toUnsignedInt code >>= getVariByteInstruction (ops mode) 
  where
    ops Set = (Set1ByteChar, Set2ByteChar, Set3ByteChar, Set4ByteChar)
    ops Put = (Put1ByteChar, Put2ByteChar,  Put3ByteChar, Put4ByteChar)

getRuleInstruction :: MoveMode -> Int -> Int -> EncodableInstruction
getRuleInstruction mode h w =
  let op = case mode of
        Put -> PutRule
        Set -> SetRule
  in EncodableInstruction op $ fmap (IntArgVal . U4 . fromIntegral) [w, h]

getMoveInstruction :: Axis -> Int -> Either String EncodableInstruction
getMoveInstruction d dist =
  toSignedInt dist >>= getVariByteInstruction (ops d)
  where
    ops Vertical = (Down1Byte, Down2Byte, Down3Byte, Down4Byte)
    ops Horizontal = (Right1Byte, Right2Byte, Right3Byte, Right4Byte)

dviFormatArg, numeratorArg, denominatorArg :: ArgVal
dviFormatArg = IntArgVal $ U1 2
-- Define a fraction by which all dimensions should be multiplied to get
-- lengths in units of 10^(-7) meters.
numeratorArg = IntArgVal $ S4 $ 254 * (10 ^ (5 :: Int))
denominatorArg = IntArgVal $ S4 $ 7227 * (2 ^ (16 :: Int))

magnificationArg :: Int -> ArgVal
magnificationArg = IntArgVal . S4 . fromIntegral

ambleArgs :: Int -> [ArgVal]
ambleArgs mag = [numeratorArg, denominatorArg, magnificationArg mag]

getPreambleInstr :: Int -> EncodableInstruction
getPreambleInstr mag =
  EncodableInstruction Preamble $
  [dviFormatArg] ++ ambleArgs mag ++ [IntArgVal $ U1 0, StringArgVal ""]

getPostambleInstr :: [Int] -> Int -> Int -> Int -> Int -> EncodableInstruction
getPostambleInstr beginPagePointers mag maxPageHeightPlusDepth maxPageWidth maxStackDepth =
  EncodableInstruction Postamble $
  [lastPointerArg] ++
  ambleArgs mag ++
  [ (IntArgVal . S4 . fromIntegral) maxPageHeightPlusDepth
  , (IntArgVal . S4 . fromIntegral) maxPageWidth
  , (IntArgVal . U2 . fromIntegral) maxStackDepth
  , (IntArgVal . U2 . fromIntegral . length) beginPagePointers
  ]
  where
    lastPointerArg = (IntArgVal . S4 . fromIntegral) $ lastDef (-1) beginPagePointers

getPostPostambleInstr :: Int -> EncodableInstruction
getPostPostambleInstr postamblePointer =
  EncodableInstruction PostPostamble $
  [(IntArgVal . S4 . fromIntegral) postamblePointer, dviFormatArg] ++ replicate 4 (IntArgVal $ U1 223)
