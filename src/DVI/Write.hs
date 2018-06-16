{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances, ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}

module DVI.Write where

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BLS
import Data.Char (ord)
import qualified Data.Int as I
import qualified Data.Word as W
import System.FilePath (splitFileName)

import qualified TFM.Main as TFMM

type Fourple a = (a, a, a, a)

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

data ArgVal
  = U1 W.Word8
  | U2 W.Word16
  | U4 W.Word32
  | S1 I.Int8
  | S2 I.Int16
  | S4 I.Int32
  | S String
  deriving (Show)

data Argument = Argument
  { name :: String
  , val :: ArgVal
  } deriving (Show)

data EncodableInstruction = EncodableInstruction
  { op :: Operation
  , arguments :: [Argument]
  } deriving (Show)

class Encodable a where
  encLength :: a -> Int
  encLength = fromIntegral . BLS.length . encode
  encode :: a -> BLS.ByteString

instance Encodable ArgVal where
  encode (U1 v) = B.encode v
  encode (U2 v) = B.encode v
  encode (U4 v) = B.encode v
  encode (S1 v) = B.encode v
  encode (S2 v) = B.encode v
  encode (S4 v) = B.encode v
  encode (S v) = BLS.pack $ fmap (fromIntegral . ord) v

instance Encodable Operation where
  encode = B.encode . (fromIntegral :: Int -> W.Word8) . fromEnum

instance Encodable Argument where
  encode = encode . val

instance Encodable EncodableInstruction where
  encode instr = (encode $ op instr) `BLS.append` (encode $ arguments instr)

instance Encodable a => Encodable [a] where
  encode = BLS.concat . fmap encode

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

buildIntArgVal :: Bool -> Int -> Either String ArgVal
buildIntArgVal signed n = do
  nrBytesNeeded <- getBytesNeeded signed n
  case nrBytesNeeded of
    1 ->
      return $
      if signed
        then S1 $ fromIntegral n
        else U1 $ fromIntegral n
    2 ->
      return $
      if signed
        then S2 $ fromIntegral n
        else U2 $ fromIntegral n
    3 -> return $ S4 $ fromIntegral n
    4 -> return $ S4 $ fromIntegral n
    b -> fail $ "Cannot handle this number of bytes: " ++ show b

pickSizeOp :: Fourple a -> Bool -> Int -> Either String (a, ArgVal)
pickSizeOp (op1, op2, _, op4) signed n = do
  argVal <- buildIntArgVal signed n
  _op <-
    case argVal of
      U1 _ -> return op1
      S1 _ -> return op1
      U2 _ -> return op2
      S2 _ -> return op2
      U4 _ -> return op4
      S4 _ -> return op4
      v -> fail $ "No operation to go with this argument value: " ++ show v
  return (_op, argVal)

pickSizeOpUnsigned :: Fourple a -> Int -> Either String (a, ArgVal)
pickSizeOpUnsigned ops = pickSizeOp ops False

getVarByteInstruction :: Fourple Operation -> String -> Int -> Bool -> Either String EncodableInstruction
getVarByteInstruction ops _name n signed = do
  (_op, argVal) <- pickSizeOp ops signed n
  let arg = Argument {name = _name, val = argVal}
  return EncodableInstruction {op = _op, arguments = [arg]}

-- Encoding abstract instructions.
longSelectFontOps :: Fourple Operation
longSelectFontOps = (Select1ByteFontNr, Select2ByteFontNr, Select3ByteFontNr, Select4ByteFontNr)

getSelectFontNrInstruction :: Int -> Either String EncodableInstruction
getSelectFontNrInstruction fNr = do
  (longOp, argVal) <- pickSizeOpUnsigned longSelectFontOps fNr
  let shortOp = toEnum $ fNr + fromEnum SelectFontNr0
      fontNrArg = Argument {name = "font_number", val = argVal}
      (_op, args) =
        if fNr < 64
          then (shortOp, [])
          else (longOp, [fontNrArg])
  return EncodableInstruction {op = _op, arguments = args}

getSimpleEncInstruction :: Operation -> EncodableInstruction
getSimpleEncInstruction _op = EncodableInstruction {op = _op, arguments = []}

endPageInstruction :: EncodableInstruction
endPageInstruction = getSimpleEncInstruction EndPage

pushInstruction :: EncodableInstruction
pushInstruction = getSimpleEncInstruction Push

popInstruction :: EncodableInstruction
popInstruction = getSimpleEncInstruction Pop

lastOrMinus1 :: [Int] -> Int
lastOrMinus1 [] = -1
lastOrMinus1 a = last a

getBeginPageInstruction :: Int -> EncodableInstruction
getBeginPageInstruction lastBeginPoint =
  let boringArg n = Argument {name = 'c' : show n, val = S4 $ fromIntegral n}
      boringArgs = fmap boringArg ([0 .. 9] :: [Int])
      lastBeginPointArg =
        Argument
        { name = "last_begin_page_pointer"
        , val = S4 $ fromIntegral lastBeginPoint
        }
      args = boringArgs ++ [lastBeginPointArg]
  in EncodableInstruction {op = BeginPage, arguments = args}

-- Define font.
defineFontOps :: Fourple Operation
defineFontOps = (Define1ByteFontNr, Define2ByteFontNr, Define3ByteFontNr, Define4ByteFontNr)

-- Ugly convenience to ease checking if an operation defines a font.
defineFontOpsLst :: [Operation]
defineFontOpsLst =
    let (w, x, y, z) = defineFontOps
    in [w, x, y, z]


getDefineFontInstruction ::
     Int -> FilePath -> Int -> Int -> Int -> Either String EncodableInstruction
getDefineFontInstruction fNr fPath scaleFactor designSize fontChecksum = do
  (_op, fontNrArgVal) <- pickSizeOpUnsigned defineFontOps fNr
  let (dirPathRaw, fileName) = splitFileName fPath
      dirPath =
        if dirPathRaw == "./"
          then ""
          else dirPathRaw
      args =
        [ Argument {name = "font_nr", val = fontNrArgVal}
        , Argument {name = "checksum", val = U4 $ fromIntegral fontChecksum}
        , Argument {name = "scale_factor", val = S4 $ fromIntegral scaleFactor}
        , Argument {name = "design_size", val = S4 $ fromIntegral designSize}
        , Argument
          {name = "dir_path_length", val = U1 $ fromIntegral $ length dirPath}
        , Argument
          {name = "file_name_length", val = U1 $ fromIntegral $ length fileName}
        , Argument {name = "font_path", val = S fPath}
        ]
  if fPath == (dirPath ++ fileName)
    then return ()
    else fail $
         "Split path badly: " ++
         fPath ++ " not equal to (" ++ dirPath ++ ", " ++ fileName ++ ")"
  return EncodableInstruction {op = _op, arguments = args}

-- Put or set characters.
setCharOps :: Fourple Operation
setCharOps = (Set1ByteChar, Set2ByteChar, Set3ByteChar, Set4ByteChar)

putCharOps :: Fourple Operation
putCharOps = (Put1ByteChar, Put2ByteChar, Put3ByteChar, Put4ByteChar)

getCharacterInstruction :: Int -> Bool -> Either String EncodableInstruction
getCharacterInstruction code True = do
  (longOp, charNrArgVal) <- pickSizeOpUnsigned setCharOps code
  let shortOp = toEnum $ code + fromEnum SetChar0
      charNrArg = Argument {name = "char_nr", val = charNrArgVal}
      (_op, args) =
        if code < 128
          then (shortOp, [])
          else (longOp, [charNrArg])
  return EncodableInstruction {op = _op, arguments = args}
getCharacterInstruction code False = getVarByteInstruction putCharOps "char_nr" code False

-- Encode abstract instructions.
data Instruction
  = Character { charNr :: Int
              , move :: Bool }
  | Rule { height :: Int
         , width :: Int
         , move :: Bool }
  | BeginNewPage
  | MoveRight { distance :: Int }
  | MoveRightW
  | MoveRightX
  | MoveDown { distance :: Int }
  | MoveDownY
  | MoveDownZ
    -- Fonts.
  | DefineFont { fontInfo :: TFMM.TexFont
               , fontPath :: FilePath
               , fontNr :: Int
               , scaleFactorRatio :: Rational }
  | SelectFont { fontNr :: Int }
    -- Other.
  | PushStack
  | PopStack
  | DoSpecial { cmd :: String }
  deriving (Show)

moveRightOps :: Fourple Operation
moveRightOps = (Right1Byte, Right2Byte, Right3Byte, Right4Byte)

moveDownOps :: Fourple Operation
moveDownOps = (Down1Byte, Down2Byte, Down3Byte, Down4Byte)

getMoveInstruction :: Bool -> Int -> Either String EncodableInstruction
getMoveInstruction right dist = getVarByteInstruction (if right then moveRightOps else moveDownOps) "distance" dist True

-- History: (Encodable instructions, selected font number, begin-page pointers, stack depth, max stack depth)
encodeInstructions :: [Instruction] -> Int -> Either String ([EncodableInstruction], Maybe Int, [Int], Int, Int)
encodeInstructions [] magnification = Right ([getPreambleInstr magnification], Nothing, [], 0, 0)
encodeInstructions (this:rest) magnification = do
  (restEncodeds, restCurrentFontNr, restBeginPagePointers, restStackDepth, restMaxStackDepth) <-
    encodeInstructions rest magnification
  (thisEncodeds, thisCurrentFontNr, thisBeginPagePointers) <-
    case this of
      SelectFont {fontNr = _fontNr} ->
        case getSelectFontNrInstruction _fontNr of
          Left s -> fail s
          Right instr -> return ([instr], Just _fontNr, restBeginPagePointers)
      Character {charNr = _charNr, move = _move} -> do
        charInstr <- getCharacterInstruction _charNr _move
        return ([charInstr], restCurrentFontNr, restBeginPagePointers)
      Rule {width = w, height = h, move = _move} ->
        let _op = if _move then SetRule else PutRule
            args =
              [ Argument {name = "height", val = U4 $ fromIntegral h}
              , Argument {name = "width", val = U4 $ fromIntegral w}
              ]
            instr = EncodableInstruction {op = _op, arguments = args}
        in return ([instr], restCurrentFontNr, restBeginPagePointers)
      BeginNewPage -> do
        let endRet =
              case restBeginPagePointers of
                [] -> []
                _ -> [endPageInstruction]
            beginPageInstr =
              getBeginPageInstruction $ lastOrMinus1 restBeginPagePointers
            newBeginPagePointer = encLength (endRet ++ restEncodeds)
        fontRet <-
          case restCurrentFontNr of
            Just nr ->
              case getSelectFontNrInstruction nr of
                Left s -> fail s
                Right instr -> return [instr]
            Nothing -> return []
        return
          ( fontRet ++ [beginPageInstr] ++ endRet
          , restCurrentFontNr
          , newBeginPagePointer : restBeginPagePointers)
      MoveRight {distance = dist} -> do
        instr <- getMoveInstruction True dist
        return ([instr], restCurrentFontNr, restBeginPagePointers)
      MoveRightW {} -> fail "Not implemented"
      MoveRightX {} -> fail "Not implemented"
      MoveDown {distance = dist} -> do
        instr <- getMoveInstruction False dist
        return ([instr], restCurrentFontNr, restBeginPagePointers)
      MoveDownY {} -> fail "Not implemented"
      MoveDownZ {} -> fail "Not implemented"
      DefineFont { fontInfo = info
                 , fontPath = path
                 , fontNr = nr
                 , scaleFactorRatio = scaleRatio
                 } -> do
        let checksum = TFMM.checksum info
            designSize = round $ TFMM.designSizeSP info
            scaleFactor = TFMM.designScaleSP info scaleRatio
        defineFontInstruction <-
          getDefineFontInstruction nr path scaleFactor designSize checksum
        return
          ([defineFontInstruction], restCurrentFontNr, restBeginPagePointers)
      PushStack ->
        return ([pushInstruction], restCurrentFontNr, restBeginPagePointers)
      PopStack ->
        return ([popInstruction], restCurrentFontNr, restBeginPagePointers)
      DoSpecial {} -> fail "Not implemented"
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

-- Document.
dviFormat :: Int
dviFormat = 2

numerator :: Int
numerator = 254 * (10 ^ (5 :: Int))

denominator :: Int
denominator = 7227 * (2 ^ (16 :: Int))

comment :: String
comment = ""

signatureInteger :: Int
signatureInteger = 223

dviFormatArg :: Argument
dviFormatArg = Argument {name = "dvi_format", val = U1 $ fromIntegral dviFormat}

numeratorArg :: Argument
numeratorArg = Argument {name = "numerator", val = S4 $ fromIntegral numerator}

denominatorArg :: Argument
denominatorArg = Argument {name = "denominator", val = S4 $ fromIntegral denominator}

magnificationArg :: Int -> Argument
magnificationArg magnification = Argument {name = "magnification", val = S4 $ fromIntegral magnification}

getPreambleInstr :: Int -> EncodableInstruction
getPreambleInstr magnification =
  EncodableInstruction
  { op = Preamble
  , arguments =
      [ dviFormatArg
        -- Define a fraction by which all dimensions should be multiplied to get
        -- lengths in units of 10^(-7) meters.
      , numeratorArg
      , denominatorArg
      , magnificationArg magnification
      , Argument
        {name = "comment_length", val = U1 $ fromIntegral $ length comment}
      , Argument {name = "comment", val = S comment}
      ]
  }

getPostambleInstr :: [Int] -> Int -> Int -> Int -> Int -> EncodableInstruction
getPostambleInstr beginPagePointers magnification maxPageHeightPlusDepth maxPageWidth maxStackDepth =
  EncodableInstruction
  { op = Postamble
  , arguments =
      [ Argument
        { name = "final_begin_page_pointer"
        , val = S4 $ fromIntegral $ lastOrMinus1 beginPagePointers
        }
      , numeratorArg
      , denominatorArg
      , magnificationArg magnification
      , Argument
        { name = "max_page_height_plus_depth"
        , val = S4 $ fromIntegral maxPageHeightPlusDepth
        }
      , Argument {name = "max_page_width", val = S4 $ fromIntegral maxPageWidth}
      , Argument
        {name = "max_stack_depth", val = U2 $ fromIntegral maxStackDepth}
      , Argument
        {name = "nr_pages", val = U2 $ fromIntegral $ length beginPagePointers}
      ]
  }

getPostPostambleInstr :: Int -> EncodableInstruction
getPostPostambleInstr postamblePointer =
  let signature =
        replicate
          4
          Argument
          {name = "signature", val = U1 $ fromIntegral signatureInteger}
  in EncodableInstruction
     { op = PostPostamble
     , arguments =
         [ Argument
           { name = "postamble_pointer"
           , val = S4 $ fromIntegral postamblePointer
           }
         , dviFormatArg
         ] ++
         signature
     }

encodeDocument :: [Instruction] -> Int -> Either String [EncodableInstruction]
encodeDocument instrs magnification = do
  (mundaneInstrs, _, beginPagePointers, _, maxStackDepth) <-
    encodeInstructions instrs magnification
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
      fontDefinitions = filter (\instr -> op instr `elem` defineFontOpsLst) mundaneInstrs
  return $
    [postPostambleInstr] ++
    fontDefinitions ++ [postambleInstr] ++ finishedInstrs
