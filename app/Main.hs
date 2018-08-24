{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (writeFile)

import Data.Char (ord)
import Data.ByteString.Lazy (writeFile)
import Control.Monad.Trans.State.Lazy (runStateT)
import Data.Maybe
import System.Console.GetOpt
import System.Environment

import DVI.Encode (buildDocument, encode)

import HeX.Box.Draw (toDVI)
import HeX.Build (extractPages, newCurrentPage)
import HeX.Config (newConfig)
import HeX.Parse (newStream)

data Flag
 = Help
 | Output FilePath
 deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]    (NoArg Help)            "show usage information"
  , Option ['o'] ["output"]  (OptArg output "FILE")  "output to FILE"
  ]
  where
    output = Output . fromMaybe "out.dvi"

usage :: String
usage = usageInfo header options
  where header = "Usage: hex [OPTION...] file"

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argStr = 
  case getOpt Permute options argStr of
     (o, n, []) -> return (o, n)
     (_, _, errs) -> ioError $ userError $ concat errs ++ usage

run :: FilePath -> FilePath -> IO ()
run inFName outFName = do
  stream <- newStream . fmap ord <$> readFile inFName

  conf <- newConfig
  ((pages, _), _) <- runStateT (extractPages [] newCurrentPage [] stream) conf

  let instrs = toDVI pages
  case buildDocument instrs 1000 of
    Left err -> ioError $ userError err
    Right encInstrs ->  writeFile outFName $ encode $ reverse encInstrs

main :: IO ()
main = do
  getArgs >>= parseArgs >>= \case
    ([Help], []) -> putStrLn usage
    ([], [inFName]) -> run inFName "out.dvi"
    ([Output outFName], [inFName]) -> run inFName outFName
    (_, _) -> ioError $ userError usage
  return ()
