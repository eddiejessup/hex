module DVI.Operation where

import HeXlude

import qualified Data.Binary                   as B
import qualified Data.ByteString.Lazy          as BS.L
import qualified Data.Word                     as W

import           HeX.Type

import           DVI.Encode

data ByteLength
    = OneByte
    | TwoByte
    | ThreeByte
    | FourByte
    deriving (Show)

toWord :: ByteLength -> W.Word8
toWord OneByte = 0
toWord TwoByte = 1
toWord ThreeByte = 2
toWord FourByte = 3

data CharOp
    = FastCharOp W.Word8
    | ArgCharOp MoveMode ByteLength
    deriving (Show)

data VarOp
    = KeepVar
    | ChangeVar ByteLength
    deriving (Show)

data MoveOp
    = ArgMoveOp ByteLength
    | VarMoveOp MoveVar VarOp
    deriving (Show)

data MoveVar
    = WOrY
    | XOrZ
    deriving (Show)

data SelectFontOp
    = FastSelectFontOp W.Word8
    | ArgSelectFontOp ByteLength
    deriving (Show)

data Operation
    = AddChar CharOp
    | AddRule MoveMode
    | NoOp
    | BeginPage
    | EndPage
    | Push
    | Pop
    | Move Axis MoveOp
    | SelectFontNr SelectFontOp
    | DoSpecial ByteLength
    | DefineFontNr ByteLength
    | Preamble
    | Postamble
    | PostPostamble
    deriving (Show)

instance Encodable Operation where
    encode op = BS.L.toStrict $ B.encode $ case op of
        AddChar (FastCharOp n) -> n
        AddChar (ArgCharOp Set b) -> 128 + toWord b
        AddChar (ArgCharOp Put b) -> 133 + toWord b
        AddRule Set -> 132
        AddRule Put -> 137
        NoOp -> 138
        BeginPage -> 139
        EndPage -> 140
        Push -> 141
        Pop -> 142
        Move Horizontal (ArgMoveOp b) -> 143 + toWord b
        Move Vertical (ArgMoveOp b) -> 157 + toWord b
        Move ax (VarMoveOp v vop) ->
            let origin = case (ax, v) of
                    (Horizontal, WOrY) -> 147
                    (Horizontal, XOrZ) -> 152
                    (Vertical, WOrY) -> 161
                    (Vertical, XOrZ) -> 166
            in case vop of
                KeepVar -> origin
                ChangeVar b -> origin + 1 + toWord b
        SelectFontNr (FastSelectFontOp n) -> 171 + n
        SelectFontNr (ArgSelectFontOp b) -> 235 + toWord b
        DoSpecial b -> 239 + toWord b
        DefineFontNr b -> 243 + toWord b
        Preamble -> 247
        Postamble -> 248
        PostPostamble -> 249
