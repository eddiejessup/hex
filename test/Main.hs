module Main where

import Protolude hiding (yield)
import Test.Tasty
import qualified Hex.Test.Categorise as Categorise
import qualified Hex.Test.Lex as Lex

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Test"
    [ Categorise.tests,
      Lex.tests
    ]
