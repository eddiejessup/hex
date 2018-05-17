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
import qualified Box
import qualified Paragraph

main = do
    contents <- readFile "test.tex"

    let
        -- Add in some useful extras beyond the technical defaults.
        extras = [('^', Cat.Superscript)]
        usableMap = foldl (\m (k, v) -> Map.insert k v m) Cat.defaultCharCatMap extras
        charToCat = Cat.toCatCode usableMap

    -- Char-cat some stuff.
    -- print $ Cat.process charToCat contents

    -- Get some tokens.
    let charCats = Cat.process charToCat contents
    let tokens = Lex.process charCats

    -- putStrLn $ List.intercalate "\n" $ fmap show tokens

    let (vListElems, remain) = Paragraph.extractVElems tokens
    let vList = Paragraph.VVList Paragraph.VList{contents=vListElems, desiredLength=Paragraph.Natural}
    let vBoxElems = Box.setVList vList

    -- putStrLn $ List.intercalate "\n" $ fmap show vBoxElems

    instrs <- Box.toDVI vBoxElems

    putStrLn $ List.intercalate "\n" $ fmap show (instrs)

    let Right encInstrs = DVIW.encodeDocument (reverse instrs) 1000

    -- putStrLn $ List.intercalate "\n" $ fmap show (reverse encInstrs)

    BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

    return ()
