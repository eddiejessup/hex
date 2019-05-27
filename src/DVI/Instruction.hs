module DVI.Instruction where

import           HeXlude              hiding ( U1 )

import           Data.Ascii           ( Ascii )
import qualified Data.Ascii           as Asc
import qualified Data.Binary          as B
import           Data.Byte
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Int             as I
import qualified Data.Path            as D.Path
import qualified Data.Word            as W
import           Path                 ( Path )
import qualified Path

import           DVI.Encode
import           DVI.Operation

data ArgVal = IntArgVal IntArgVal | StringArgVal Ascii
    deriving ( Show )

instance Encodable ArgVal where
    encode (IntArgVal v) = encode v
    encode (StringArgVal v) = Asc.toByteString v

data IntArgVal =
      U1 W.Word8
    | U2 W.Word16
    | U4 W.Word32
    | S1 I.Int8
    | S2 I.Int16
    | S4 I.Int32
    deriving ( Show )

intArgValFromSignableInt :: SignableInt -> Either Text IntArgVal
intArgValFromSignableInt n@(SignableInt s i) = case (s, bytesNeeded n) of
    (Signed, 1)   -> pure $ S1 $ fromIntegral i
    (Signed, 2)   -> pure $ S2 $ fromIntegral i
    (Signed, 3)   -> pure $ S4 $ fromIntegral i
    (Signed, 4)   -> pure $ S4 $ fromIntegral i
    (Unsigned, 1) -> pure $ U1 $ fromIntegral i
    (Unsigned, 2) -> pure $ U2 $ fromIntegral i
    (Unsigned, 3) -> pure $ U4 $ fromIntegral i
    (Unsigned, 4) -> pure $ U4 $ fromIntegral i
    (_, b)        -> Left $ "Cannot handle " <> show b <> " bytes"

instance Encodable IntArgVal where
    encode a = BS.L.toStrict $
        case a of
            U1 v -> B.encode v
            U2 v -> B.encode v
            U4 v -> B.encode v
            S1 v -> B.encode v
            S2 v -> B.encode v
            S4 v -> B.encode v

data EncodableInstruction = EncodableInstruction Operation [ArgVal]
    deriving ( Show )

instance Encodable EncodableInstruction where
    encode (EncodableInstruction op args) = encode op `BS.append` encode args

instance Readable EncodableInstruction where
    describe = show

opByteLength :: IntArgVal -> ByteLength
opByteLength v = case v of
    (S1 _) -> OneByte
    (U1 _) -> OneByte
    (S2 _) -> TwoByte
    (U2 _) -> TwoByte
    (S4 _) -> FourByte
    (U4 _) -> FourByte

getVariByteOpAndArg :: (ByteLength -> Operation)
                    -> SignableInt
                    -> Either Text (Operation, ArgVal)
getVariByteOpAndArg f sI = do
    iArgVal <- intArgValFromSignableInt sI
    pure (f $ opByteLength iArgVal, IntArgVal iArgVal)

getVariByteInstruction
    :: (ByteLength -> Operation)
    -> SignableInt
    -> Either Text EncodableInstruction
getVariByteInstruction f sI = do
    (op, arg) <- getVariByteOpAndArg f sI
    pure $ EncodableInstruction op [ arg ]

getSimpleEncInstruction :: Operation -> EncodableInstruction
getSimpleEncInstruction _op = EncodableInstruction _op []

getSelectFontNrInstruction :: Int -> Either Text EncodableInstruction
getSelectFontNrInstruction fNr
    | fNr < 64 = pure $
        getSimpleEncInstruction $
        SelectFontNr $
        FastSelectFontOp $
        fromIntegral fNr
    | otherwise =
        toUnsignedInt fNr >>= getVariByteInstruction (SelectFontNr . ArgSelectFontOp)

endPageInstruction, pushInstruction, popInstruction :: EncodableInstruction
endPageInstruction = getSimpleEncInstruction EndPage

pushInstruction = getSimpleEncInstruction Push

popInstruction = getSimpleEncInstruction Pop

getBeginPageInstruction :: Int -> EncodableInstruction
getBeginPageInstruction lastBeginPoint =
    let boringArgs = fmap (IntArgVal . S4 . fromIntegral) ([ 0 .. 9 ] :: [Int])
        lastBeginPointArg = (IntArgVal . S4 . fromIntegral) lastBeginPoint
        args = boringArgs <> [ lastBeginPointArg ]
    in
        EncodableInstruction BeginPage args

getDefineFontInstruction
    :: Int
    -> Path b Path.File
    -> Int
    -> Int
    -> Int
    -> Either Text EncodableInstruction
getDefineFontInstruction fNr path scaleFactor designSize fontChecksum = do
    sI <- toUnsignedInt fNr
    (_op, fontNrArgVal) <- getVariByteOpAndArg DefineFontNr sI
    dirPathStr <- pathToAscii (Path.parent path) <&> stripLeadingDot
    fileNameAscii <- D.Path.fileNameText path >>= textToAscii
    let args =
            [ fontNrArgVal  -- font_nr
            , (IntArgVal . U4 . fromIntegral) fontChecksum  -- checksum
            , (IntArgVal . S4 . fromIntegral) scaleFactor  -- scale_factor
            , (IntArgVal . S4 . fromIntegral) designSize  -- design_size
            , (IntArgVal . U1 . fromIntegral . asciiLength) dirPathStr  -- dir_path_length
            , (IntArgVal . U1 . fromIntegral . asciiLength) fileNameAscii  -- file_name_length
            , StringArgVal $ dirPathStr <> fileNameAscii  -- font_path
            ]
    pure $ EncodableInstruction _op args
  where
    liftMaybeAscii v = liftMaybe ("Could not represent as ASCII: " <> show v) v

    pathToAscii p = Asc.fromChars (Path.toFilePath p) & liftMaybeAscii

    textToAscii t = Asc.fromText t & liftMaybeAscii

    asciiLength = BS.length . Asc.toByteString

    stripLeadingDot x = if Asc.toText x == "./"
                        then Asc.unsafeFromText ""
                        else x

getCharacterInstruction :: Int -> MoveMode -> Either Text EncodableInstruction
getCharacterInstruction code mode = case mode of
    Set
        | code < 128 -> pure $
            getSimpleEncInstruction $
            AddChar $
            FastCharOp $
            fromIntegral code
    _   ->
        toUnsignedInt code >>= getVariByteInstruction (AddChar . ArgCharOp mode)

getRuleInstruction :: MoveMode -> Int -> Int -> EncodableInstruction
getRuleInstruction mode h w =
    EncodableInstruction (AddRule mode) (IntArgVal . U4 . fromIntegral <$> [h, w])

getMoveInstruction :: Axis -> Int -> Either Text EncodableInstruction
getMoveInstruction ax dist =
    toSignedInt dist >>= getVariByteInstruction (Move ax . ArgMoveOp)

dviFormatArg, numeratorArg, denominatorArg :: ArgVal
dviFormatArg = IntArgVal $ U1 2

-- Define a fraction by which all dimensions should be multiplied to get
-- lengths in units of 10^(-7) meters.
numeratorArg = IntArgVal $ S4 $ 254 * (10 ^ (5 :: Int))

denominatorArg = IntArgVal $ S4 $ 7227 * (2 ^ (16 :: Int))

ambleArgs :: Int -> [ArgVal]
ambleArgs mag =
    [ numeratorArg, denominatorArg, (IntArgVal . S4 . fromIntegral) mag ]

getPreambleInstr :: Int -> EncodableInstruction
getPreambleInstr mag = EncodableInstruction Preamble args
  where
    args = [ dviFormatArg ]
        <> ambleArgs mag
        <> [ IntArgVal $ U1 0, StringArgVal "" ]

getPostambleInstr :: [Int] -> Int -> Int -> Int -> Int -> EncodableInstruction
getPostambleInstr beginPagePointers
                  mag
                  maxPageHeightPlusDepth
                  maxPageWidth
                  maxStackDepth = EncodableInstruction Postamble args
  where
    args = [ (IntArgVal . S4 . fromIntegral) lastPointerArg ]
        <> ambleArgs mag
        <> [ (IntArgVal . S4 . fromIntegral) maxPageHeightPlusDepth
           , (IntArgVal . S4 . fromIntegral) maxPageWidth
           , (IntArgVal . U2 . fromIntegral) maxStackDepth
           , (IntArgVal . U2 . fromIntegral . length) beginPagePointers
           ]

    lastPointerArg = lastDef (-1) beginPagePointers

getPostPostambleInstr :: Int -> EncodableInstruction
getPostPostambleInstr postamblePointer =
    EncodableInstruction PostPostamble args
  where
    args = [ (IntArgVal . S4 . fromIntegral) postamblePointer, dviFormatArg ]
        <> signature

    signature = replicate 4 (IntArgVal $ U1 223)
