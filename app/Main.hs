{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.IntMap.Strict as IMap
import qualified Data.Char as C
import qualified Data.ByteString.Lazy as BLS
import qualified Data.List as List

import qualified DVI.Write as DVIW
import qualified Cat
import qualified Lex
import qualified Box
import qualified Parse
import qualified Command

main :: IO ()
main = do
  contents <- readFile "test.tex"

  let contentsCode = fmap C.ord contents

  let
      -- Add in some useful extras beyond the technical defaults.
      extras = [(94, Cat.Superscript)]
      usableMap = foldl (\m (k, v) -> IMap.insert k v m) Cat.defaultCharCatMap extras

  -- Get some char-cats.
  let charCats = Cat.extractAll usableMap contentsCode

  -- Get some tokens.
  let tokens = Lex.extractAll usableMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show tokens

  -- Get some commands.
  let coms = Command.extractAll usableMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show coms

  let state = Parse.State {currentFontInfo=Nothing}

  pages <- Parse.extractPages state [] usableMap Lex.LineBegin contentsCode

  -- putStrLn $ show $ pages !! 0

  let instrs = Box.toDVI pages
  let Right encInstrs = DVIW.encodeDocument (reverse instrs) 1000
  BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

  return ()
