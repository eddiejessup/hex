module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import Hex.Categorise
import qualified Hex.Command.Run as Run
import Hex.Config.Config
import Hex.Lex
import Hex.Parse.Stream.Expanding (newExpandStream)
import Hex.Resolve hiding (Output)
import Hexlude
import qualified Path
import qualified Path.IO
import qualified System.Console.GetOpt as Opt
import qualified Hex.App as App

data Flag
  = Help
  | Amble
  | Output FilePath
  | Mode Run.Mode
  deriving stock (Show, Eq)

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
      Nothing -> Mode Run.DVIBytesMode
      Just modeStr -> case Run.readMode modeStr of
        Just m -> Mode m
        Nothing -> panic $ "Unknown mode: " <> toS modeStr

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
  (inputRaw, maybeInPath) <-
    case args of
      ["-"] -> do
        cs <- liftIO BS.L.getContents
        pure (cs, Nothing)
      [inPathStr] -> do
        path <- Path.IO.resolveFile' (toS inPathStr)
        cs <- BS.L.readFile (Path.toFilePath path)
        pure (cs, Just path)
      _ ->
        panic "Please specify a file to execute, or '-' to read from the standard-input stream"
  let input =
        if Amble `elem` flags
        then preamble <> inputRaw <> postamble
        else inputRaw
      mode = lastDef Run.DVIBytesMode [m | Mode m <- flags]

  conf <- newConfig (maybeToList (Path.parent <$> maybeInPath))

  let s = newExpandStream maybeInPath input
  case lastMay [f | Output f <- flags] of
    Nothing ->
      renderWithMode conf input mode >>= \case
        Left appError ->
          putText $ show appError
        Right tx ->
          putText tx
    Just destPathStr -> case mode of
      Run.DVIBytesMode ->
        App.runApp (Run.streamToDVIBytes s) conf >>= \case
          Left appError ->
            putText $ show appError
          Right bytes -> do
            putText $ "Writing to " <> toS destPathStr <> "..."
            BS.writeFile destPathStr bytes
      _ ->
        panic "Writing non-DVI-bytes mode to file not supported"

renderWithMode
  :: forall m
   . MonadIO m
  => Config
  -> BS.L.ByteString
  -> Run.Mode
  -> m (Either App.AppError Text)
renderWithMode conf input = \case
  Run.CatMode -> pure $ Right $ show $ usableCodesToCharCats input -- TODO: Render nicely.
  Run.LexMode -> pure $ Right $ show $ usableCodesToLexTokens input
  Run.ResolveMode -> pure $ Right $ show $ usableCodesToResolvedTokens input
  Run.ExpandMode -> Run.expandingStreamAsPrimTokens @m @App.AppError s conf <&> undefined
  Run.CommandMode -> Run.expandingStreamAsCommands @m @App.AppError s conf <&> undefined
  Run.ParaListMode -> App.runApp (Run.renderStreamUnsetPara s) conf
  Run.ParaSetMode -> App.runApp (Run.renderStreamSetPara s) conf
  Run.PageListMode -> App.runApp (Run.renderStreamPageList s) conf
  Run.PageMode -> App.runApp (Run.renderStreamPages s) conf
  Run.SemanticDVIMode -> App.runApp (Run.renderStreamSemanticDVI s) conf
  Run.RawDVIMode -> App.runApp (Run.renderStreamRawDVI s) conf
  Run.DVIBytesMode -> panic "You probably don't want to render the DVI bytes as text"
  where
    s = newExpandStream Nothing input
