module Main where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import Hex.Parse.Stream.Expanding (newExpandStream)
import Hexlude
import qualified Path
import qualified Hex.Command.Run as Run
import qualified Path.IO
import qualified System.Console.GetOpt as Opt

data Flag
  = Help
  | Amble
  | Output !FilePath
  | Mode !Mode
  deriving (Show, Eq)

options :: [Opt.OptDescr Flag]
options =
  [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "show usage information"
  , Opt.Option ['a'] ["amble"] (Opt.NoArg Amble) "prepend pre- and post-amble to input"
  , Opt.Option ['o'] ["output"] (Opt.OptArg output "FILE") "output to FILE"
  , Opt.Option ['m'] ["mode"] (Opt.OptArg mode "MODE") "output in mode MODE"
  ]
  where
    output = Output . toS . fromMaybe "out.dvi"

    mode = \case
      Nothing -> DVIWriteMode
      Just modeStr -> case readMode modeStr of
        Just m -> m
        Nothing -> panic $ "Unknown mode: " <> modeStr

usage :: Text
usage = toS $ Opt.usageInfo header options
  where
    header = "Usage: hex [OPTION...] [file]"

preamble :: BS.L.ByteString
preamble = "\\font\\thefont=cmr10 \\thefont\n\n"

postamble :: BS.L.ByteString
postamble = "\n\n\\end\n"

parseArgs :: [Text] -> IO ([Flag], [Text])
parseArgs argStr = case Opt.getOpt Opt.Permute options (toS <$> argStr) of
  (o, n, []) -> pure (o, toS <$> n)
  (_, _, errs) -> panic ((mconcat (toS <$> errs) <> usage) :: Text)

main :: IO ()
main = do
  (flags, args) <- ((toS <$>) <$> getArgs) >>= parseArgs
  when (Help `elem` flags) $ panic usage
  (inputRaw, maybePath) <-
    case args of
      ["-"] -> do
        cs <- liftIO BS.L.getContents
        pure (cs, Nothing)
      [pathStr] -> do
        path <- Path.IO.resolveFile' (toS pathStr)
        cs <- BS.L.readFile (Path.toFilePath path)
        pure (cs, Just path)
      _ ->
        panic "Please specify a file to execute, or '-' to read from the standard-input stream"
  let input =
        if Amble `elem` flags
        then preamble <> inputRaw <> postamble
        else inputRaw
      makeStream = newExpandStream maybePath input
      mode = lastDef DVIWriteMode [m | Mode m <- flags]
  case lastMay [f | Output f <- flags] of
    Nothing -> printWithMode mode
    Just destPathStr ->
      case mode of
        DVIBytesMode ->
          makeStream >>= streamToDVIBytes >>= BS.writeFile destPathStr
    _ ->
      printWithMode mode input
