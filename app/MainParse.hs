{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Text.Megaparsec as P
import qualified Data.Set as Set
import Data.Proxy
import qualified Data.Char as C
import qualified Data.List as List

import qualified Harden
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
  -- putStrLn $ show $ P.parse parser "" example1
  -- putStrLn $ show $ P.parse parser "" example2
  contents <- readFile "test.tex"
  let contentsCode = fmap C.ord contents

  -- let charCats = Cat.extractAll Cat.usableCharCatMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show charCats

  -- let lexTokens = Lex.extractAll Cat.usableCharCatMap contentsCode
  -- putStrLn $ List.intercalate "\n" $ fmap show lexTokens

  -- let (Just (lexTok, _, rest)) = Lex.extractToken Cat.usableCharCatMap Lex.LineBegin contentsCode
  -- putStrLn $ show lexTok

  let stream0 = Command.Stream {codes=contentsCode, lexState=Lex.LineBegin, ccMap=Cat.usableCharCatMap}
  let ress = parcel stream0

  putStrLn $ List.intercalate "\n" $ fmap show ress

  -- putStrLn $ show $ stream0

  -- let (Right (com1, stream1)) = P.parse Command.hParser "" stream0

  -- putStrLn $ show $ com1
  -- putStrLn $ show $ stream1

  -- let (Right (com2, stream2)) = P.parse Command.hParser "" stream1

  -- putStrLn $ show $ com2
  -- putStrLn $ show $ stream2

  return ()


-- (Token s -> Either (Maybe x, Set y) a)
