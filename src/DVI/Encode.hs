module DVI.Encode where

import qualified Data.Binary                   as B
import qualified Data.ByteString.Lazy          as BLS
import           Data.Char                      ( ord )
import qualified Data.Int                      as I
import qualified Data.Word                     as W

import           Data.Byte

class Encodable a where
    encode :: a -> BLS.ByteString

encLength :: Encodable a => a -> Int
encLength = fromIntegral . BLS.length . encode

encLengths :: Encodable a => Functor f => f a -> f Int
encLengths = fmap encLength

encStarts :: Encodable a => [a] -> [Int]
encStarts = scanl (+) 0 . encLengths

encEnds :: Encodable a => [a] -> [Int]
encEnds = scanl1 (+) . encLengths

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
