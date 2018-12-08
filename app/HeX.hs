module Main where

import           Prelude                 hiding ( writeFile )

import           Data.ByteString.Lazy           ( writeFile )
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           Safe                           ( lastDef )
import           Control.Monad                  ( when )
import           HeX.Run

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
  deriving (Show, Eq)

data Flag
  = Help
  | Amble
  | Output FilePath
  | Mode Mode
  deriving (Show, Eq)

data Input
  = Stdin
  | File FilePath

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]   (NoArg Help)           "show usage information"
  , Option ['a'] ["amble"]  (NoArg Amble) "prepend pre- and post-amble to input"
  , Option ['o'] ["output"] (OptArg output "FILE") "output to FILE"
  , Option ['m'] ["mode"]   (OptArg mode "MODE")   "output in mode MODE"
  ]
 where
  output = Output . fromMaybe "out.dvi"

  mode (Just "cat"     ) = Mode CatMode
  mode (Just "lex"     ) = Mode LexMode
  mode (Just "resolve" ) = Mode ResolveMode
  mode (Just "expand"  ) = Mode ExpandMode
  mode (Just "command" ) = Mode CommandMode
  mode (Just "paralist") = Mode ParaListMode
  mode (Just "paraset" ) = Mode ParaSetMode
  mode (Just "page"    ) = Mode PageMode
  mode (Just "dvi"     ) = Mode DVIMode
  mode (Just "rawdvi"  ) = Mode RawDVIMode
  mode (Just "bytes"   ) = Mode DVIWriteMode
  mode (Just m         ) = error $ "Unknown mode: " ++ m
  mode Nothing           = Mode DVIWriteMode

usage :: String
usage = usageInfo header options
  where header = "Usage: hex [OPTION...] [file]"

preamble, postamble :: String
preamble = "\\font\\thefont=cmr10 \\selectfont\n\n"
postamble = "\n\n\\end\n"

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argStr = case getOpt Permute options argStr of
  (o, n, []  ) -> pure (o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usage

main :: IO ()
main = do
  (flags, args) <- getArgs >>= parseArgs

  when (Help `elem` flags) (ioError $ userError usage)

  inputRaw <- case args of
    ["-"] -> getContents
    [f  ] -> readFile f
    _     -> ioError $ userError usage

  let input = if Amble `elem` flags
        then preamble ++ inputRaw ++ postamble
        else inputRaw

  let mode = lastDef DVIWriteMode [ m | (Mode m) <- flags ]
  let dest = lastDef "out.dvi" [ f | (Output f) <- flags ]

  case mode of
    CatMode      -> runCat input
    LexMode      -> runLex input
    ResolveMode  -> runResolved input
    ExpandMode   -> runExpand input
    CommandMode  -> runCommand input
    ParaListMode -> runPara input
    ParaSetMode  -> runSetPara input
    PageMode     -> runPages input
    DVIMode      -> runDVI input
    RawDVIMode   -> runDVIRaw input
    DVIWriteMode -> codesToDVIBytes input >>= writeFile dest
