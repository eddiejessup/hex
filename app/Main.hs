{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Char as C
import qualified Data.ByteString.Lazy as BLS

import qualified DVI.Encode as DVIE
import qualified BoxDraw
import qualified Build
import Parse.Util (newStream)

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
  -- let coms = Parse.extractAllDebug Cat.usableCharCatMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show coms

  let stream = newStream contentsCode

  -- page <- Build.extractPage Build.newState [] stream

  (_, pages, _) <- Build.extractPages Build.newState [] Build.newCurrentPage [] stream

  -- putStrLn $ show $ pages !! 0

  let instrs = BoxDraw.toDVI $ pages
  let Right encInstrs = DVIE.encodeDocument (reverse instrs) 1000
  BLS.writeFile "out.dvi" $ DVIE.encode $ reverse encInstrs

  return ()
