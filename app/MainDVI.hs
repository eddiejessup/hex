{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import Data.ByteString.Builder (toLazyByteString, lazyByteStringHex, byteStringHex)
import Data.Binary.Strict.Get as BSG
import Data.Binary (encode)
import Data.List (intercalate)
import qualified Data.Word as W
import qualified Data.Int as I

import qualified DVI.Write as DVIW

import qualified TFM.Main as TFMM

showEncArg arg = "\t" ++ (DVIW.name arg) ++ " = " ++ (show $ DVIW.val arg) ++ "\n"
showEncInstr instr =
    let
        instrS = (show (DVIW.op instr)) ++ " (" ++ (show $ DVIW.encLength instr) ++ ")"
        argsS = concat $ fmap showEncArg (DVIW.arguments instr)
    in
        instrS ++ "\n" ++ argsS
showEncInstrs instrs = intercalate "\n" $ fmap showEncInstr $ reverse instrs

main = do
    -- -- let ans = encode (-2 :: W.Word16)
    -- let shower = toLazyByteString . lazyByteStringHex
    -- -- let encshow = shower . encode
    -- -- print $ fmap encshow ([2, -2] :: [I.Int16])
    -- -- print $ fmap encshow ([2, -2] :: [I.Int8])
    -- -- print $ fmap encshow (["hihi", "hoho"])
    -- -- print $ shower $ encode (-2 :: I.Int16)
    -- let v = DVIW.S "hih"
    -- -- let v = DVIW.EncodableArg { DVIW.name="hihi", DVIW.val="yo" }
    -- print v
    -- print $ shower $ DVIW.encode v

    -- let a = DVIW.Argument { DVIW.name="hihi", DVIW.val=DVIW.S "hih" }
    -- print a
    -- print $ shower $ DVIW.encode a

    -- let op = DVIW.BeginPage
    -- print op
    -- print $ shower $ DVIW.encode op

    -- let args = [a, a]
    -- print args
    -- print $ shower $ DVIW.encode args

    -- let instr = DVIW.Instruction { op=op, arguments=args }
    -- print instr
    -- print $ shower $ DVIW.encodeInstr instr
    -- print $ shower $ DVIW.encode instr
    -- print $ DVIW.encLength instr

    -- print $ DVIW.encStarts [instr]

    -- print $ DVIW.instrsAndPoints [instr, instr]
    -- print $ DVIW.beginPage dviDoc

    let fontPath = "cmr10"
    Right fontInfo <- TFMM.readTFM "cmr10.tfm"
    -- let (Right dviDocF) = DVIW.defineFont fontInfo fontPath 1 1.0 dviDoc

    let instrs = [ DVIW.Character {charNr=90, move=True}
                 , DVIW.SelectFont 1
                 , DVIW.DefineFont {fontInfo=fontInfo, fontPath=fontPath, fontNr=1, scaleFactorRatio=1.0}
                 , DVIW.BeginNewPage ]
    let Right encInstrs = DVIW.encodeDocument instrs 1000

    putStrLn $ showEncInstrs encInstrs
    BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

    return ()
