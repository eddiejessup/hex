module DVI.Instruction where

import qualified Data.Binary                   as B
import qualified Data.ByteString.Lazy          as BLS
import           Data.Char                      ( ord )
import qualified Data.Int                      as I
import qualified Data.Word                     as W
import           Safe                           ( lastDef )
import           System.FilePath                ( splitFileName )

import           Data.Concept
import           Data.Byte

import           DVI.Encode
import           DVI.Operation

data ArgVal
    = IntArgVal IntArgVal
    | StringArgVal String
    deriving (Show)

instance Encodable ArgVal where
    encode (IntArgVal    v) = encode v
    encode (StringArgVal v) = BLS.pack $ fmap (fromIntegral . ord) v

data IntArgVal
    = U1 W.Word8
    | U2 W.Word16
    | U4 W.Word32
    | S1 I.Int8
    | S2 I.Int16
    | S4 I.Int32
    deriving (Show)

intArgValFromSignableInt :: SignableInt -> Either String IntArgVal
intArgValFromSignableInt n@(SignableInt s i) = case (s, bytesNeeded n) of
    (Signed  , 1) -> pure $ S1 $ fromIntegral i
    (Signed  , 2) -> pure $ S2 $ fromIntegral i
    (Signed  , 3) -> pure $ S4 $ fromIntegral i
    (Signed  , 4) -> pure $ S4 $ fromIntegral i
    (Unsigned, 1) -> pure $ U1 $ fromIntegral i
    (Unsigned, 2) -> pure $ U2 $ fromIntegral i
    (Unsigned, 3) -> pure $ U4 $ fromIntegral i
    (Unsigned, 4) -> pure $ U4 $ fromIntegral i
    (_       , b) -> fail $ "Cannot handle " ++ show b ++ " bytes"

instance Encodable IntArgVal where
    encode (U1 v) = B.encode v
    encode (U2 v) = B.encode v
    encode (U4 v) = B.encode v
    encode (S1 v) = B.encode v
    encode (S2 v) = B.encode v
    encode (S4 v) = B.encode v

data EncodableInstruction = EncodableInstruction Operation [ArgVal]
    deriving (Show)

instance Encodable EncodableInstruction where
    encode (EncodableInstruction op args) = encode op `BLS.append` encode args

opByteLength :: IntArgVal -> ByteLength
opByteLength v = case v of
    (S1 _) -> OneByte
    (U1 _) -> OneByte
    (S2 _) -> TwoByte
    (U2 _) -> TwoByte
    (S4 _) -> FourByte
    (U4 _) -> FourByte

getVariByteOpAndArg
    :: (ByteLength -> Operation)
    -> SignableInt
    -> Either String (Operation, ArgVal)
getVariByteOpAndArg f sI =
    do
    iArgVal <- intArgValFromSignableInt sI
    pure (f $ opByteLength iArgVal, IntArgVal iArgVal)

getVariByteInstruction
    :: (ByteLength -> Operation)
    -> SignableInt
    -> Either String EncodableInstruction
getVariByteInstruction f sI =
    do
    (op, arg) <- getVariByteOpAndArg f sI
    pure $ EncodableInstruction op [arg]

getSimpleEncInstruction :: Operation -> EncodableInstruction
getSimpleEncInstruction _op = EncodableInstruction _op []

getSelectFontNrInstruction :: Int -> Either String EncodableInstruction
getSelectFontNrInstruction fNr
    | fNr < 64 =
        pure $ getSimpleEncInstruction $ SelectFontNr $ FastSelectFontOp $ fromIntegral fNr
    | otherwise =
        do
        arg <- toUnsignedInt fNr
        getVariByteInstruction (\b -> SelectFontNr $ ArgSelectFontOp b) arg

endPageInstruction, pushInstruction, popInstruction :: EncodableInstruction
endPageInstruction = getSimpleEncInstruction EndPage
pushInstruction = getSimpleEncInstruction Push
popInstruction = getSimpleEncInstruction Pop

getBeginPageInstruction :: Int -> EncodableInstruction
getBeginPageInstruction lastBeginPoint =
    let
        boringArgs = fmap (IntArgVal . S4 . fromIntegral) ([0 .. 9] :: [Int])
        lastBeginPointArg = (IntArgVal . S4 . fromIntegral) lastBeginPoint
        args = boringArgs ++ [lastBeginPointArg]
    in
        EncodableInstruction BeginPage args

getDefineFontInstruction
  :: Int
  -> FilePath
  -> Int
  -> Int
  -> Int
  -> Either String EncodableInstruction
getDefineFontInstruction fNr fPath scaleFactor designSize fontChecksum =
    do
    sI <- toUnsignedInt fNr
    (_op, fontNrArgVal) <- getVariByteOpAndArg DefineFontNr sI
    let (dirPathRaw, fileName) = splitFileName fPath
        dirPath = if dirPathRaw == "./"
            then ""
            else dirPathRaw
        args =
            [ fontNrArgVal  -- font_nr
            , (IntArgVal . U4 . fromIntegral) fontChecksum  -- checksum
            , (IntArgVal . S4 . fromIntegral) scaleFactor  -- scale_factor
            , (IntArgVal . S4 . fromIntegral) designSize  -- design_size
            , (IntArgVal . U1 . fromIntegral . length) dirPath  -- dir_path_length
            , (IntArgVal . U1 . fromIntegral . length) fileName  -- file_name_length
            , StringArgVal fPath  -- font_path
            ]
    pure $ EncodableInstruction _op args

getCharacterInstruction :: Int -> MoveMode -> Either String EncodableInstruction
getCharacterInstruction code mode =
    case mode of
        Set | code < 128 ->
            pure $ getSimpleEncInstruction $ AddChar $ FastCharOp $ fromIntegral code
        _ ->
            do
            arg <- toUnsignedInt code
            getVariByteInstruction (\b -> AddChar $ ArgCharOp mode b) arg

getRuleInstruction :: MoveMode -> Int -> Int -> EncodableInstruction
getRuleInstruction mode h w =
  EncodableInstruction (AddRule mode) $ fmap (IntArgVal . U4 . fromIntegral) [h, w]

getMoveInstruction :: Axis -> Int -> Either String EncodableInstruction
getMoveInstruction ax dist =
    do
    arg <- toSignedInt dist
    getVariByteInstruction (\b -> Move ax $ ArgMoveOp b) arg

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
