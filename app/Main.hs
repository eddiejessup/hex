{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.IntMap.Strict as IMap
import qualified Data.Char as C
import qualified Data.ByteString.Lazy as BLS

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
  let tokens = Lex.extractAllInit charCats

  -- Get some commands.
  let commands = Command.extractAll tokens

  let state = Parse.State {currentFontInfo=Nothing}

  (_, pages) <- Parse.extractPages state commands []

  -- putStrLn $ List.intercalate "\n" $ fmap show vBoxElems

  let instrs = Box.toDVI pages

  -- putStrLn $ List.intercalate "\n" $ fmap show instrs

  let Right encInstrs = DVIW.encodeDocument (reverse instrs) 1000

  -- -- putStrLn $ List.intercalate "\n" $ fmap show (reverse encInstrs)

  BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

  return ()
