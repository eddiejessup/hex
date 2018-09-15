{-# LANGUAGE LambdaCase #-}

module Main where

import HeX.Categorise
import Prelude hiding (writeFile)

import Data.ByteString.Lazy (writeFile, ByteString)
import Data.Maybe
import System.Console.GetOpt
import System.Environment

import HeX.Run

data Mode
  = CatMode
  | LexMode
  | ResolveMode
  | ExpandMode
  | CommandMode
  | ParaListMode
  | ParaSetMode
  | PageMode
  | DVIMode
  | RawDVIMode
  | DVIWriteMode
  deriving (Show)

data Flag
  = Help
  | Output FilePath
  | Mode Mode
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['o'] ["output"] (OptArg output "FILE") "output to FILE"
  , Option ['m'] ["mode"] (OptArg mode "MODE") "output in mode MODE"
  ]
  where
    output = Output . fromMaybe "out.dvi"

    mode (Just "cat") = Mode CatMode
    mode (Just "lex") = Mode LexMode
    mode (Just "resolve") = Mode ResolveMode
    mode (Just "expand") = Mode ExpandMode
    mode (Just "command") = Mode CommandMode
    mode (Just "paralist") = Mode ParaListMode
    mode (Just "paraset") = Mode ParaSetMode
    mode (Just "page") = Mode PageMode
    mode (Just "dvi") = Mode DVIMode
    mode (Just "rawdvi") = Mode RawDVIMode
    mode (Just "bytes") = Mode DVIWriteMode
    mode (Just m) = error $ "Unknown mode: " ++ m
    mode Nothing = Mode DVIWriteMode

usage :: String
usage = usageInfo header options
  where
    header = "Usage: hex [OPTION...] file"

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argStr =
  case getOpt Permute options argStr of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError $ userError $ concat errs ++ usage


run :: FilePath -> FilePath -> ([CharCode] -> IO ByteString) -> IO ()
run inFName outFName fn =
  readFile inFName >>= fn >>= writeFile outFName

main :: IO ()
main = do
  getArgs >>= parseArgs >>= \case
    ([Help], []) -> putStrLn usage
    ([], [inFName]) -> run inFName "out.dvi" codesToDVIBytes
    ([Mode CatMode], [inFName]) -> readFile inFName >>= runCat
    ([Mode LexMode], [inFName]) -> readFile inFName >>= runLex
    ([Mode ResolveMode], [inFName]) -> readFile inFName >>= runResolved
    ([Mode ExpandMode], [inFName]) -> readFile inFName >>= runExpand
    ([Mode CommandMode], [inFName]) -> readFile inFName >>= runCommand
    ([Mode ParaListMode], [inFName]) -> readFile inFName >>= runPara
    ([Mode ParaSetMode], [inFName]) -> readFile inFName >>= runSetPara
    ([Mode PageMode], [inFName]) -> readFile inFName >>= runPages
    ([Mode DVIMode], [inFName]) -> readFile inFName >>= runDVI
    ([Mode RawDVIMode], [inFName]) -> readFile inFName >>= runDVIRaw
    ([Mode DVIWriteMode, Output outFName], [inFName]) -> run inFName outFName codesToDVIBytes
    (_, _) -> ioError $ userError usage
  return ()
