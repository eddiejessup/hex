{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Char as C
import qualified Data.ByteString.Lazy as BLS
import Control.Monad.Trans.State.Lazy (runStateT)

import qualified DVI.Encode as DVIE
import Box.Draw (toDVI)
import qualified Build
import qualified Config
import Expand (defaultCSMap)
import Parse.Stream (newExpandStream)

main :: IO ()
main = do
  contents <- readFile "test.tex"
  let contentsCode = fmap C.ord contents

  let stream = newExpandStream contentsCode defaultCSMap

  ((pages, _), _) <- runStateT (Build.extractPages [] Build.newCurrentPage [] stream) Config.newConfig

  let instrs = toDVI pages
  let Right encInstrs = DVIE.encodeDocument (reverse instrs) 1000
  BLS.writeFile "out.dvi" $ DVIE.encode $ reverse encInstrs

  return ()
