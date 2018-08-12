{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.ByteString.Lazy as BLS
import Data.ByteString.Builder (toLazyByteString, lazyByteStringHex)
import Data.Binary (encode)
import Data.List (intercalate)
import qualified Data.Word as W
import qualified Data.Int as I

import qualified DVI.Encode as DVIE

import qualified TFM

showEncArg arg = "\t" ++ (DVIE.name arg) ++ " = " ++ (show $ DVIE.val arg) ++ "\n"
showEncInstr instr =
    let
        instrS = (show (DVIE.op instr)) ++ " (" ++ (show $ DVIE.encLength instr) ++ ")"
        argsS = concat $ fmap showEncArg (DVIE.arguments instr)
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
    -- let v = DVIE.S "hih"
    -- -- let v = DVIE.EncodableArg { DVIE.name="hihi", DVIE.val="yo" }
    -- print v
    -- print $ shower $ DVIE.encode v

    -- let a = DVIE.Argument { DVIE.name="hihi", DVIE.val=DVIE.S "hih" }
    -- print a
    -- print $ shower $ DVIE.encode a

    -- let op = DVIE.BeginPage
    -- print op
    -- print $ shower $ DVIE.encode op

    -- let args = [a, a]
    -- print args
    -- print $ shower $ DVIE.encode args

    -- let instr = DVIE.Instruction { op=op, arguments=args }
    -- print instr
    -- print $ shower $ DVIE.encodeInstr instr
    -- print $ shower $ DVIE.encode instr
    -- print $ DVIE.encLength instr

    -- print $ DVIE.encStarts [instr]

    -- print $ DVIE.instrsAndPoints [instr, instr]
    -- print $ DVIE.beginPage dviDoc

    let fontPath = "cmr10"
    fontInfo <- TFM.readTFM "cmr10.tfm"
    -- let (Right dviDocF) = DVIE.defineFont fontInfo fontPath 1 1.0 dviDoc

    let instrs = [ DVIE.Character{charNr=80, move=True}
                 , DVIE.PopStack
                 , DVIE.PushStack
                 , DVIE.PushStack
                 , DVIE.MoveDown{distance=20000000}
                 , DVIE.Rule{height=100000, width=5000000, move=True}
                 , DVIE.MoveRight{distance=5000000}
                 , DVIE.Character{charNr=90, move=True}
                 , DVIE.SelectFont 1
                 , DVIE.DefineFont{fontInfo=fontInfo, fontPath=fontPath, fontNr=1, scaleFactorRatio=1.0}
                 , DVIE.BeginNewPage ]
    let Right encInstrs = DVIE.encodeDocument instrs 1000

    putStrLn $ showEncInstrs encInstrs
    BLS.writeFile "out.dvi" $ DVIE.encode $ reverse encInstrs

    return ()
