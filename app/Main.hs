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
import qualified Parse
import qualified Setting
import qualified Command

main = do
  contents <- readFile "test.tex"

  let
      -- Add in some useful extras beyond the technical defaults.
      extras = [('^', Cat.Superscript)]
      usableMap = foldl (\m (k, v) -> Map.insert k v m) Cat.defaultCharCatMap extras
      charToCat = Cat.toCatCode usableMap

  -- Get some char-cats.
  let charCats = Cat.process charToCat contents

  -- Get some tokens.
  let tokens = Lex.process charCats

  -- Get some commands.
  let commands = Command.process tokens

  let state = Parse.State {currentFontInfo=Nothing}

  (stateEnd, pages) <- Parse.extractPages state commands []

  -- putStrLn $ List.intercalate "\n" $ fmap show vBoxElems

  let instrs = Box.toDVI pages

  -- putStrLn $ List.intercalate "\n" $ fmap show instrs

  let Right encInstrs = DVIW.encodeDocument (reverse instrs) 1000

  -- -- putStrLn $ List.intercalate "\n" $ fmap show (reverse encInstrs)

  BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

  return ()
