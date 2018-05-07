{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as C
import qualified Data.ByteString.Lazy as BLS

import qualified DVI.Write as DVIW
import qualified TFM.Main as TFMM
import qualified Cat
import qualified Lex

writeLexTokens :: [Lex.LexToken] -> [DVIW.Instruction]
writeLexTokens [] = []
writeLexTokens (Lex.CharCat Cat.CharCat{char=char, cat=cat}:rest)
    | cat `elem` [Cat.Letter, Cat.Other] = DVIW.Character{charNr=C.ord char, move=True}:writeLexTokens rest
    | cat == Cat.Space = DVIW.MoveRight{distance=300000}:writeLexTokens rest
    | otherwise = writeLexTokens rest
writeLexTokens (Lex.ControlSequenceCall{name=name}:rest)
    | name == "par" = DVIW.PopStack:DVIW.MoveDown{distance=1000000}:DVIW.PushStack:writeLexTokens rest
    | otherwise = writeLexTokens rest


main = do
    contents <- readFile "test.tex"

    let
        -- Add in some useful extras beyond the technical defaults.
        extras = [('^', Cat.Superscript)]
        usableMap = foldl (\m (k, v) -> Map.insert k v m) Cat.defaultCharCatMap extras
        charToCat = Cat.toCatCode usableMap

    -- Char-cat some stuff.
    print $ Cat.process charToCat contents

    -- Get some tokens.
    let charCats = Cat.process charToCat contents
    let tokens = Lex.process charCats
    putStrLn $ List.intercalate "\n" $ fmap show tokens

    let fontPath = "cmr10"
    Right fontInfo <- TFMM.readTFM "cmr10.tfm"

    let initInstrs = [ DVIW.BeginNewPage
                     , DVIW.PushStack
                     , DVIW.DefineFont{fontInfo=fontInfo, fontPath=fontPath, fontNr=1, scaleFactorRatio=1.0}
                     , DVIW.SelectFont 1 ]

    let meatInstrs = writeLexTokens tokens
    let instrs = initInstrs ++ meatInstrs

    putStrLn $ List.intercalate "\n" $ fmap show (meatInstrs)

    -- -- Get a document
    -- let instrs = [ DVIW.Character{charNr=80, move=True}
    --              , DVIW.PopStack
    --              , DVIW.PushStack
    --              , DVIW.PushStack
    --              , DVIW.MoveDown{distance=20000000}
    --              , DVIW.Rule{height=100000, width=5000000, move=True}
    --              , DVIW.MoveRight{distance=5000000}
    --              , DVIW.Character{charNr=90, move=True}
    --              , DVIW.SelectFont 1
    --              , DVIW.DefineFont{fontInfo=fontInfo, fontPath=fontPath, fontNr=1, scaleFactorRatio=1.0}
    --              , DVIW.BeginNewPage ]
    let Right encInstrs = DVIW.encodeDocument (reverse instrs) 1000

    BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

    return ()
