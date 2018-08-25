{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module DVI.Encode where

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BLS
import Data.Char (ord)
import qualified Data.Int as I
import qualified Data.Word as W
import Safe (lastDef)
import System.FilePath (splitFileName)

class Encodable a where
  encLength :: a -> Int
  encLength = fromIntegral . BLS.length . encode
  encode :: a -> BLS.ByteString

encLengths :: (Functor f, Encodable a) => f a -> f Int
encLengths = fmap encLength

lagCumSum :: (Num a) => [a] -> [a]
lagCumSum = scanl (+) 0

cumSum :: (Num a) => [a] -> [a]
cumSum = scanl1 (+)

encStarts :: Encodable a => [a] -> [Int]
encStarts = lagCumSum . encLengths

encEnds :: Encodable a => [a] -> [Int]
encEnds = cumSum . encLengths

instance Encodable a => Encodable [a] where
  encode = BLS.concat . fmap encode

data Operation
    -- 0 to 127: set that character number.
  = SetChar0
  | SetChar1
  | SetChar2
  | SetChar3
  | SetChar4
  | SetChar5
  | SetChar6
  | SetChar7
  | SetChar8
  | SetChar9
  | SetChar10
  | SetChar11
  | SetChar12
  | SetChar13
  | SetChar14
  | SetChar15
  | SetChar16
  | SetChar17
  | SetChar18
  | SetChar19
  | SetChar20
  | SetChar21
  | SetChar22
  | SetChar23
  | SetChar24
  | SetChar25
  | SetChar26
  | SetChar27
  | SetChar28
  | SetChar29
  | SetChar30
  | SetChar31
  | SetChar32
  | SetChar33
  | SetChar34
  | SetChar35
  | SetChar36
  | SetChar37
  | SetChar38
  | SetChar39
  | SetChar40
  | SetChar41
  | SetChar42
  | SetChar43
  | SetChar44
  | SetChar45
  | SetChar46
  | SetChar47
  | SetChar48
  | SetChar49
  | SetChar50
  | SetChar51
  | SetChar52
  | SetChar53
  | SetChar54
  | SetChar55
  | SetChar56
  | SetChar57
  | SetChar58
  | SetChar59
  | SetChar60
  | SetChar61
  | SetChar62
  | SetChar63
  | SetChar64
  | SetChar65
  | SetChar66
  | SetChar67
  | SetChar68
  | SetChar69
  | SetChar70
  | SetChar71
  | SetChar72
  | SetChar73
  | SetChar74
  | SetChar75
  | SetChar76
  | SetChar77
  | SetChar78
  | SetChar79
  | SetChar80
  | SetChar81
  | SetChar82
  | SetChar83
  | SetChar84
  | SetChar85
  | SetChar86
  | SetChar87
  | SetChar88
  | SetChar89
  | SetChar90
  | SetChar91
  | SetChar92
  | SetChar93
  | SetChar94
  | SetChar95
  | SetChar96
  | SetChar97
  | SetChar98
  | SetChar99
  | SetChar100
  | SetChar101
  | SetChar102
  | SetChar103
  | SetChar104
  | SetChar105
  | SetChar106
  | SetChar107
  | SetChar108
  | SetChar109
  | SetChar110
  | SetChar111
  | SetChar112
  | SetChar113
  | SetChar114
  | SetChar115
  | SetChar116
  | SetChar117
  | SetChar118
  | SetChar119
  | SetChar120
  | SetChar121
  | SetChar122
  | SetChar123
  | SetChar124
  | SetChar125
  | SetChar126
  | SetChar127
  | Set1ByteChar
  | Set2ByteChar
  | Set3ByteChar
  | Set4ByteChar
  | SetRule
  | Put1ByteChar
  | Put2ByteChar
  | Put3ByteChar
  | Put4ByteChar
  | PutRule
  | NoOp
  | BeginPage
  | EndPage
  | Push
  | Pop
  | Right1Byte
  | Right2Byte
  | Right3Byte
  | Right4Byte
  | RightW
  | Set1ByteWThenRightW
  | Set2ByteWThenRightW
  | Set3ByteWThenRightW
  | Set4ByteWThenRightW
  | RightX
  | Set1ByteXThenRightX
  | Set2ByteXThenRightX
  | Set3ByteXThenRightX
  | Set4ByteXThenRightX
  | Down1Byte
  | Down2Byte
  | Down3Byte
  | Down4Byte
  | DownY
  | Set1ByteYThenDownY
  | Set2ByteYThenDownY
  | Set3ByteYThenDownY
  | Set4ByteYThenDownY
  | DownZ
  | Set1ByteZThenDownZ
  | Set2ByteZThenDownZ
  | Set3ByteZThenDownZ
  | Set4ByteZThenDownZ
    -- 171 to 234: Select font number.
  | SelectFontNr0
  | SelectFontNr1
  | SelectFontNr2
  | SelectFontNr3
  | SelectFontNr4
  | SelectFontNr5
  | SelectFontNr6
  | SelectFontNr7
  | SelectFontNr8
  | SelectFontNr9
  | SelectFontNr10
  | SelectFontNr11
  | SelectFontNr12
  | SelectFontNr13
  | SelectFontNr14
  | SelectFontNr15
  | SelectFontNr16
  | SelectFontNr17
  | SelectFontNr18
  | SelectFontNr19
  | SelectFontNr20
  | SelectFontNr21
  | SelectFontNr22
  | SelectFontNr23
  | SelectFontNr24
  | SelectFontNr25
  | SelectFontNr26
  | SelectFontNr27
  | SelectFontNr28
  | SelectFontNr29
  | SelectFontNr30
  | SelectFontNr31
  | SelectFontNr32
  | SelectFontNr33
  | SelectFontNr34
  | SelectFontNr35
  | SelectFontNr36
  | SelectFontNr37
  | SelectFontNr38
  | SelectFontNr39
  | SelectFontNr40
  | SelectFontNr41
  | SelectFontNr42
  | SelectFontNr43
  | SelectFontNr44
  | SelectFontNr45
  | SelectFontNr46
  | SelectFontNr47
  | SelectFontNr48
  | SelectFontNr49
  | SelectFontNr50
  | SelectFontNr51
  | SelectFontNr52
  | SelectFontNr53
  | SelectFontNr54
  | SelectFontNr55
  | SelectFontNr56
  | SelectFontNr57
  | SelectFontNr58
  | SelectFontNr59
  | SelectFontNr60
  | SelectFontNr61
  | SelectFontNr62
  | SelectFontNr63
  | Select1ByteFontNr
  | Select2ByteFontNr
  | Select3ByteFontNr
  | Select4ByteFontNr
  | Do1ByteSpecial
  | Do2ByteSpecial
  | Do3ByteSpecial
  | Do4ByteSpecial
  | Define1ByteFontNr
  | Define2ByteFontNr
  | Define3ByteFontNr
  | Define4ByteFontNr
  | Preamble
  | Postamble
  | PostPostamble
  deriving (Show, Enum, Eq)

instance Encodable Operation where
  encode = B.encode . (fromIntegral :: Int -> W.Word8) . fromEnum

data ArgVal
  = U1 W.Word8
  | U2 W.Word16
  | U4 W.Word32
  | S1 I.Int8
  | S2 I.Int16
  | S4 I.Int32
  | S String
  deriving (Show)

instance Encodable ArgVal where
  encode (U1 v) = B.encode v
  encode (U2 v) = B.encode v
  encode (U4 v) = B.encode v
  encode (S1 v) = B.encode v
  encode (S2 v) = B.encode v
  encode (S4 v) = B.encode v
  encode (S v) = BLS.pack $ fmap (fromIntegral . ord) v

data EncodableInstruction =
  EncodableInstruction Operation
                       [ArgVal]
  deriving (Show)

instance Encodable EncodableInstruction where
  encode (EncodableInstruction op args) = encode op `BLS.append` encode args

-- Byte picking.
isSignedNrExpressibleInNBits :: Int -> Int -> Bool
isSignedNrExpressibleInNBits n nrBits =
  let minSignedVal = -(2 ^ (nrBits - 1))
      maxSignedVal = 2 ^ (nrBits - 1) - 1
  in (minSignedVal <= n) && (n <= maxSignedVal)

getBytesNeededUnsigned :: Int -> Int
getBytesNeededUnsigned n =
  1 + floor ((logBase 256.0 $ fromIntegral $ abs n) :: Double)

getBytesNeeded :: Bool -> Int -> Either String Int
getBytesNeeded _ 0 = return 1
getBytesNeeded signedRaw n
  | (n < 0) && not signedRaw =
    fail "Cannot encode negative number in unsigned byte string"
  | otherwise =
    let nrBytesUnsigned = getBytesNeededUnsigned n
            -- 4 byte arguments are always signedRaw.
        signed = signedRaw || (nrBytesUnsigned == 4)
        nrBitsUnsigned = 8 * nrBytesUnsigned
        needExtraByte =
          signed && not (isSignedNrExpressibleInNBits n nrBitsUnsigned)
        nrBytes =
          nrBytesUnsigned +
          if needExtraByte
            then 1
            else 0
    in return nrBytes

pickSizeOp :: a -> a -> a -> a -> Bool -> Int -> Either String (a, ArgVal)
pickSizeOp op1 op2 _ op4 signed n =
  getBytesNeeded signed n >>= \case
    1 -> do
      let v =
            if signed
              then S1 $ fromIntegral n
              else U1 $ fromIntegral n
      return (op1, v)
    2 -> do
      let v =
            if signed
              then S2 $ fromIntegral n
              else U2 $ fromIntegral n
      return (op2, v)
    3 -> do
      let v = S4 $ fromIntegral n
      return (op4, v)
    4 -> do
      let v = S4 $ fromIntegral n
      return (op4, v)
    b -> fail $ "Cannot handle this number of bytes: " ++ show b

pickSizeOpUnsigned :: a -> a -> a -> a -> Int -> Either String (a, ArgVal)
pickSizeOpUnsigned op1 op2 op3 op4 = pickSizeOp op1 op2 op3 op4 False

getVarByteInstruction ::
     Operation
  -> Operation
  -> Operation
  -> Operation
  -> Int
  -> Bool
  -> Either String EncodableInstruction
getVarByteInstruction op1 op2 op3 op4 n signed = do
  (_op, argVal) <- pickSizeOp op1 op2 op3 op4 signed n
  return $ EncodableInstruction _op [argVal]

getSelectFontNrInstruction :: Int -> Either String EncodableInstruction
getSelectFontNrInstruction fNr = do
  (longOp, argVal) <-
    pickSizeOpUnsigned
      Select1ByteFontNr
      Select2ByteFontNr
      Select3ByteFontNr
      Select4ByteFontNr
      fNr
  let shortOp = toEnum $ fNr + fromEnum SelectFontNr0
      (_op, args) =
        if fNr < 64
          then (shortOp, [])
          else (longOp, [argVal])
  return $ EncodableInstruction _op args

getSimpleEncInstruction :: Operation -> EncodableInstruction
getSimpleEncInstruction _op = EncodableInstruction _op []

endPageInstruction :: EncodableInstruction
endPageInstruction = getSimpleEncInstruction EndPage

pushInstruction :: EncodableInstruction
pushInstruction = getSimpleEncInstruction Push

popInstruction :: EncodableInstruction
popInstruction = getSimpleEncInstruction Pop

getBeginPageInstruction :: Int -> EncodableInstruction
getBeginPageInstruction lastBeginPoint =
  let boringArg n = S4 $ fromIntegral n
      boringArgs = fmap boringArg ([0 .. 9] :: [Int])
      lastBeginPointArg = S4 $ fromIntegral lastBeginPoint
      args = boringArgs ++ [lastBeginPointArg]
  in EncodableInstruction BeginPage args

getDefineFontInstruction ::
     Int -> FilePath -> Int -> Int -> Int -> Either String EncodableInstruction
getDefineFontInstruction fNr fPath scaleFactor designSize fontChecksum = do
  (_op, fontNrArgVal) <-
    pickSizeOpUnsigned
      Define1ByteFontNr
      Define2ByteFontNr
      Define3ByteFontNr
      Define4ByteFontNr
      fNr
  let (dirPathRaw, fileName) = splitFileName fPath
      dirPath =
        if dirPathRaw == "./"
          then ""
          else dirPathRaw
      args =
        [ fontNrArgVal -- font_nr
        , U4 $ fromIntegral fontChecksum -- checksum
        , S4 $ fromIntegral scaleFactor -- scale_factor
        , S4 $ fromIntegral designSize -- design_size
        , U1 $ fromIntegral $ length dirPath -- dir_path_length
        , U1 $ fromIntegral $ length fileName -- file_name_length
        , S fPath -- font_path
        ]
  if fPath == (dirPath ++ fileName)
    then return ()
    else fail $
         "Split path badly: " ++
         fPath ++ " not equal to (" ++ dirPath ++ ", " ++ fileName ++ ")"
  return $ EncodableInstruction _op args

getCharacterInstruction :: Int -> Bool -> Either String EncodableInstruction
getCharacterInstruction code True = do
  (longOp, charNrArgVal) <-
    pickSizeOpUnsigned Set1ByteChar Set2ByteChar Set3ByteChar Set4ByteChar code
  let shortOp = toEnum $ code + fromEnum SetChar0
      (_op, args) =
        if code < 128
          then (shortOp, [])
          else (longOp, [charNrArgVal])
  return $ EncodableInstruction _op args
getCharacterInstruction code False =
  getVarByteInstruction
    Put1ByteChar
    Put2ByteChar
    Put3ByteChar
    Put4ByteChar
    code
    False

getMoveInstruction :: Bool -> Int -> Either String EncodableInstruction
getMoveInstruction right dist =
  (if right
     then getVarByteInstruction Right1Byte Right2Byte Right3Byte Right4Byte
     else getVarByteInstruction Down1Byte Down2Byte Down3Byte Down4Byte)
    dist
    True

dviFormatArg :: ArgVal
dviFormatArg = U1 2

-- Define a fraction by which all dimensions should be multiplied to get
-- lengths in units of 10^(-7) meters.
numeratorArg, denominatorArg :: ArgVal
numeratorArg = S4 $ 254 * (10 ^ (5 :: Int))

denominatorArg = S4 $ 7227 * (2 ^ (16 :: Int))

magnificationArg :: Int -> ArgVal
magnificationArg mag = S4 $ fromIntegral mag

ambleArgs :: Int -> [ArgVal]
ambleArgs mag = [numeratorArg, denominatorArg, magnificationArg mag]

getPreambleInstr :: Int -> EncodableInstruction
getPreambleInstr mag =
  EncodableInstruction Preamble $
  [dviFormatArg] ++ ambleArgs mag ++ [U1 0, S ""]

getPostambleInstr :: [Int] -> Int -> Int -> Int -> Int -> EncodableInstruction
getPostambleInstr beginPagePointers mag maxPageHeightPlusDepth maxPageWidth maxStackDepth =
  EncodableInstruction Postamble $
  [lastPointerArg] ++
  ambleArgs mag ++
  [ S4 $ fromIntegral maxPageHeightPlusDepth
  , S4 $ fromIntegral maxPageWidth
  , U2 $ fromIntegral maxStackDepth
  , U2 $ fromIntegral $ length beginPagePointers
  ]
  where
    lastPointerArg = S4 $ fromIntegral $ lastDef (-1) beginPagePointers

getPostPostambleInstr :: Int -> EncodableInstruction
getPostPostambleInstr postamblePointer =
  EncodableInstruction PostPostamble $
  [S4 $ fromIntegral postamblePointer, dviFormatArg] ++ replicate 4 (U1 223)
