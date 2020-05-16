module DVI.Instruction where

import DVI.Encode
import DVI.Operation
import qualified Data.Ascii as Ascii
import qualified Data.Binary as B
import Data.Byte
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Int as I
import qualified Data.Path as D.Path
import qualified Data.Word as W
import Hex.Config.Codes
import Hex.Quantity
import Hexlude hiding (U1)
import Path (Path)
import qualified Path

newtype DVIError = DVIError Text
  deriving stock (Show, Generic)

data ArgVal = UIntArgVal UIntArgVal | SIntArgVal SIntArgVal | StringArgVal Ascii.AsciiString
  deriving stock Show

instance DVIEncodable ArgVal where

  dviEncode (UIntArgVal v) = dviEncode v
  dviEncode (SIntArgVal v) = dviEncode v
  dviEncode (StringArgVal v) = Ascii.toByteString v

instance Describe ArgVal where
  describe a = singleLine $ mappend "ArgVal/" $ case a of
    UIntArgVal (U1 u) ->
      "UnsignedInt/1Byte/" <> show u
    UIntArgVal (U2 u) ->
      "UnsignedInt/2Byte/" <> show u
    UIntArgVal (U4 u) ->
      "UnsignedInt/4Byte/" <> show u
    SIntArgVal (S1 n) ->
      "SignedInt/1Byte/" <> show n
    SIntArgVal (S2 n) ->
      "SignedInt/2Byte/" <> show n
    SIntArgVal (S4 n) ->
      "SignedInt/4Byte/" <> show n
    StringArgVal s ->
      "String/" <> show s

data IntArgSize
  = B1
  | B2
  | B4
  deriving stock Show

data UIntArgVal
  = U1 W.Word8
  | U2 W.Word16
  | U4 W.Word32
  deriving stock Show

data SIntArgVal
  = S1 I.Int8
  | S2 I.Int16
  | S4 I.Int32
  deriving stock Show

uintArgValFromInt
  :: ( MonadError e m
     , AsType DVIError e

     , FiniteBits a
     , Integral a
     , Show a
     )
  => UnsignedVal a
  -> m UIntArgVal
uintArgValFromInt (UnsignedVal n) = case bytesNeededUnsigned n of
  1 -> pure $ U1 $ fromIntegral n
  2 -> pure $ U2 $ fromIntegral n
  3 -> pure $ U4 $ fromIntegral n
  4 -> pure $ U4 $ fromIntegral n
  b -> throwError $ injectTyped $ DVIError $ "Cannot represent " <> show n <> " as unsigned int: needs " <> show b <> " bytes"

sintArgValFromInt
  :: ( MonadError e m
     , AsType DVIError e

     , FiniteBits a
     , Integral a
     , Show a
     )
  => a
  -> m SIntArgVal
sintArgValFromInt n = case bytesNeededSigned n of
  1 -> pure $ S1 $ fromIntegral n
  2 -> pure $ S2 $ fromIntegral n
  3 -> pure $ S4 $ fromIntegral n
  4 -> pure $ S4 $ fromIntegral n
  b -> throwError $ injectTyped $ DVIError $ "Cannot represent " <> show n <> " as signed int: needs " <> show b <> " bytes"

instance DVIEncodable UIntArgVal where

  dviEncode a =
    BS.L.toStrict $ case a of
      U1 v -> B.encode v
      U2 v -> B.encode v
      U4 v -> B.encode v

instance DVIEncodable SIntArgVal where

  dviEncode a =
    BS.L.toStrict $ case a of
      S1 v -> B.encode v
      S2 v -> B.encode v
      S4 v -> B.encode v

data EncodableInstruction = EncodableInstruction Operation [ArgVal]
  deriving stock Show

instance DVIEncodable EncodableInstruction where

  dviEncode (EncodableInstruction op args) = dviEncode op `BS.append` dviEncode args

instance Describe EncodableInstruction where

  describe (EncodableInstruction op args) =
    [ (0, "EncodableInstruction")
    ]
    <> describeRel 1 op
    <> describeNamedRelFoldable1 "Args" args

uIntOpByteLength :: UIntArgVal -> ByteLength
uIntOpByteLength = \case
  U1 _ -> OneByte
  U2 _ -> TwoByte
  U4 _ -> FourByte

sIntOpByteLength :: SIntArgVal -> ByteLength
sIntOpByteLength = \case
  S1 _ -> OneByte
  S2 _ -> TwoByte
  S4 _ -> FourByte

getVariByteUIntOpAndArg
  :: ( MonadError e m
     , AsType DVIError e

     , FiniteBits a
     , Integral a
     , Show a
     )
  => (ByteLength -> Operation)
  -> UnsignedVal a
  -> m (Operation, UIntArgVal)
getVariByteUIntOpAndArg f n = do
  argVal <- uintArgValFromInt n
  pure (f $ uIntOpByteLength argVal, argVal)

getVariByteSIntOpAndArg
  :: ( MonadError e m
     , AsType DVIError e

     , FiniteBits a
     , Integral a
     , Show a
     )
  => (ByteLength -> Operation)
  -> a
  -> m (Operation, SIntArgVal)
getVariByteSIntOpAndArg f n = do
  argVal <- sintArgValFromInt n
  pure (f $ sIntOpByteLength argVal, argVal)

getVariByteInstruction
  :: ( MonadError e m
     , AsType DVIError e
     , AsType ByteError e

     , FiniteBits a
     , Integral a
     , Show a
     )
  => (ByteLength -> Operation)
  -> SignableInt a
  -> m EncodableInstruction
getVariByteInstruction f (SignableInt signed n) = do
  (op, arg) <-
    case signed of
      Signed -> do
        (op, sArg) <- getVariByteSIntOpAndArg f n
        pure (op, SIntArgVal sArg)
      Unsigned -> do
        uns <- toUnsigned n
        (op, uArg) <- getVariByteUIntOpAndArg f uns
        pure (op, UIntArgVal uArg)
  pure $ EncodableInstruction op [arg]

getSimpleEncInstruction :: Operation -> EncodableInstruction
getSimpleEncInstruction _op = EncodableInstruction _op []

getSelectFontNrInstruction
  :: (MonadError e m, AsType DVIError e, AsType ByteError e)
  => TeXInt
  -> m EncodableInstruction
getSelectFontNrInstruction fNr
  | fNr < 64 =
    pure $
      getSimpleEncInstruction $
      SelectFontNr $
      FastSelectFontOp $
      fromIntegral fNr
  | otherwise =
    getVariByteInstruction (SelectFontNr . ArgSelectFontOp) (SignableInt Unsigned fNr)

endPageInstruction, pushInstruction, popInstruction :: EncodableInstruction
endPageInstruction = getSimpleEncInstruction EndPage

pushInstruction = getSimpleEncInstruction Push

popInstruction = getSimpleEncInstruction Pop

getBeginPageInstruction :: Int -> EncodableInstruction
getBeginPageInstruction lastBeginPoint =
  let boringArgs = fmap (SIntArgVal . S4 . fromIntegral) ([0.. 9] :: [Int])
      lastBeginPointArg = (SIntArgVal . S4 . fromIntegral) lastBeginPoint
      args = boringArgs <> [lastBeginPointArg]
  in EncodableInstruction BeginPage args

getDefineFontInstruction
  :: (MonadError e m, AsType D.Path.PathError e, AsType ByteError e, AsType DVIError e)
  => TeXInt
  -> Path b Path.File
  -> Length
  -> Length
  -> Int
  -> m EncodableInstruction
getDefineFontInstruction fNr path scaleFactor designSize fontChecksum = do
  (_op, fontNrUArgVal) <- toUnsigned fNr >>= getVariByteUIntOpAndArg DefineFontNr
  dirPathStr <- pathToAscii (Path.parent path) <&> stripLeadingDot
  fileNameAscii <- D.Path.fileNameText path >>= textToAscii
  let args =
        [ UIntArgVal fontNrUArgVal -- font_nr
        , (UIntArgVal . U4 . fromIntegral) fontChecksum -- checksum
        , (SIntArgVal . S4 . fromIntegral) scaleFactor -- scale_factor
        , (SIntArgVal . S4 . fromIntegral) designSize -- design_size
        , (UIntArgVal . U1 . fromIntegral . asciiLength) dirPathStr -- dir_path_length
        , (UIntArgVal . U1 . fromIntegral . asciiLength) fileNameAscii -- file_name_length
        , StringArgVal $ dirPathStr <> fileNameAscii -- font_path
        ]
  pure $ EncodableInstruction _op args
  where
    noteAscii v = note (injectTyped $ D.Path.PathError $ "Could not represent as ASCII: " <> show v) v

    pathToAscii p = Ascii.fromChars (Path.toFilePath p) & noteAscii
    textToAscii t = Ascii.fromText t & noteAscii
    asciiLength = BS.length . Ascii.toByteString
    stripLeadingDot x =
      if Ascii.toText x == "./"
      then Ascii.unsafeFromText ""
      else x

getCharacterInstruction
  :: (MonadError e m, AsType DVIError e, AsType ByteError e)
  => CharCode
  -> MoveMode
  -> m EncodableInstruction
getCharacterInstruction code mode = case mode of
  Set
    | code < 128 ->
      pure $
        getSimpleEncInstruction $
        AddChar $
        FastCharOp $
        fromIntegral code
  _ ->
    getVariByteInstruction (AddChar . ArgCharOp mode) (SignableInt Unsigned code)

getRuleInstruction :: MoveMode -> Length -> Length -> EncodableInstruction
getRuleInstruction mode h w =
  EncodableInstruction (AddRule mode) (UIntArgVal . U4 . fromIntegral <$> [h, w])

getMoveInstruction
  :: (MonadError e m, AsType DVIError e, AsType ByteError e)
  => Axis
  -> Length
  -> m EncodableInstruction
getMoveInstruction ax dist =
  getVariByteInstruction (Move ax . ArgMoveOp) (SignableInt Signed (unLength dist))

dviFormatArg, numeratorArg, denominatorArg :: ArgVal
dviFormatArg = UIntArgVal $ U1 2

-- Define a fraction by which all dimensions should be multiplied to get
-- lengths in units of 10^(-7) meters.
numeratorArg = SIntArgVal $ S4 $ 254 * (10 ^ (5 :: Int))

denominatorArg = SIntArgVal $ S4 $ 7227 * (2 ^ (16 :: Int))

ambleArgs :: Int -> [ArgVal]
ambleArgs mag =
  [numeratorArg, denominatorArg, (SIntArgVal . S4 . fromIntegral) mag]

getPreambleInstr :: Int -> EncodableInstruction
getPreambleInstr mag = EncodableInstruction Preamble args
  where
    args =
      [dviFormatArg] <>
        ambleArgs mag <>
        [UIntArgVal $ U1 0, StringArgVal ""]

getPostambleInstr :: [Int] -> Int -> Int -> Int -> Int -> EncodableInstruction
getPostambleInstr beginPagePointers mag maxPageHeightPlusDepth maxPageWidth maxStackDepth =
  let lastPointerArg = lastDef (-1) beginPagePointers
      args =
        [(SIntArgVal . S4 . fromIntegral) lastPointerArg] <>
          ambleArgs mag <>
          [ (SIntArgVal . S4 . fromIntegral) maxPageHeightPlusDepth
          , (SIntArgVal . S4 . fromIntegral) maxPageWidth
          , (UIntArgVal . U2 . fromIntegral) maxStackDepth
          , (UIntArgVal . U2 . fromIntegral . length) beginPagePointers
          ]
  in EncodableInstruction Postamble args

getPostPostambleInstr :: Int -> EncodableInstruction
getPostPostambleInstr postamblePointer =
  EncodableInstruction PostPostamble args
  where
    args =
      [(SIntArgVal . S4 . fromIntegral) postamblePointer, dviFormatArg] <>
        signature
    signature = replicate 4 (UIntArgVal $ U1 223)
