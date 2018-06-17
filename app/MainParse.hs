{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Text.Megaparsec as P
import Data.Proxy
import qualified Data.Char as C
import qualified Data.List as List

import qualified Lex
import qualified Cat
import qualified Command

parcel :: Command.Stream -> [(Command.Command, Command.Stream)]
parcel stream =
  case P.parse Command.hParser "" stream of
    Left x -> []
    Right ret@(com, newStream) -> ret:parcel newStream

main :: IO ()
main = do
  contents <- readFile "test.tex"
  let contentsCode = fmap C.ord contents

  let stream0 = Command.Stream {codes=contentsCode, lexState=Lex.LineBegin, ccMap=Cat.usableCharCatMap}
  let ress = parcel stream0

  putStrLn $ List.intercalate "\n" $ fmap show ress
  return ()
