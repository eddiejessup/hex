{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.IntMap.Strict as IMAP
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

  -- Get some char-cats.
  -- let charCats = Cat.extractAll Cat.usableCharCatMap contentsCode

  -- Get some tokens.
  -- let tokens = Lex.extractAll Cat.usableCharCatMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show tokens

  -- Get some commands.
  -- let coms = Command.extractAllDebug Cat.usableCharCatMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show coms

  let stream = Command.Stream {codes=contentsCode, lexState=Lex.LineBegin, ccMap=Cat.usableCharCatMap}

  -- page <- Parse.extractPage Parse.newState [] stream

  pages <- Parse.extractPages Parse.newState [] stream

  -- -- putStrLn $ show $ pages !! 0

  let instrs = Box.toDVI pages
  let Right encInstrs = DVIW.encodeDocument (reverse instrs) 1000
  BLS.writeFile "out.dvi" $ DVIW.encode $ reverse encInstrs

  return ()
