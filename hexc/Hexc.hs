module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import qualified Hex.App as App
import Hex.Categorise
import qualified Hex.Run as Run
import Hex.Config.Config
import Hex.Lex
import Hex.Parse.Stream.Expanding
import Hex.Resolve hiding (Output, long)
import Hexlude
import qualified Path
import qualified Path.IO
import Options.Applicative
import qualified Data.Text as Tx
import Data.Conduit
import Data.Conduit.Combinators (sinkList, sourceFile, stdin, take)
import Conduit (runResourceT, MonadResource)

data Input = FileInput FilePath | StdInput

fileInputParser :: Parser Input
fileInputParser = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file")


dviRunParser :: Parser DVIWriteOptions
dviRunParser = DVIWriteOptions
  <$> strOption (  long "file" <> short 'f' <> metavar "FILENAME" <> help "Output file")

stdInputParser :: Parser Input
stdInputParser = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

inputParser :: Parser Input
inputParser = fileInputParser <|> stdInputParser

data NonExpandingMode = CatMode | LexMode | ResolveMode

data RunMode
  = NonExpandingMode NonExpandingMode
  | ExpandingMode ExpandingMode

data ExpandingMode
  = ExpandMode
  | CommandMode
  | ParaListMode
  | ParaSetMode
  | PageListMode
  | PageMode
  | SemanticDVIInstructionsMode
  | RawDVIInstructionsMode
  | DVIWriteMode DVIWriteOptions

data AppOptions = AppOptions
  { mode :: RunMode,
    input :: Input,
    searchDirs :: [FilePath],
    withAmbles :: Bool
  }

data DVIWriteOptions = DVIWriteOptions
  { dviOutputPath :: FilePath
  }

runModeParser :: Parser RunMode
runModeParser = subparser
    ( command "cat" (info (pure (NonExpandingMode CatMode)) (progDesc ""))
   <> command "lex" (info (pure (NonExpandingMode LexMode)) (progDesc ""))
   <> command "resolve" (info (pure (NonExpandingMode ResolveMode)) (progDesc ""))
   <> command "expand" (info (pure (ExpandingMode ExpandMode)) (progDesc ""))
   <> command "command" (info (pure (ExpandingMode CommandMode)) (progDesc ""))
   <> command "paralist" (info (pure (ExpandingMode ParaListMode)) (progDesc ""))
   <> command "parabox" (info (pure (ExpandingMode ParaSetMode)) (progDesc ""))
   <> command "pagelist" (info (pure (ExpandingMode PageListMode)) (progDesc ""))
   <> command "pagebox" (info (pure (ExpandingMode PageMode)) (progDesc ""))
   <> command "dvi-abstract" (info (pure (ExpandingMode SemanticDVIInstructionsMode)) (progDesc ""))
   <> command "dvi-str" (info (pure (ExpandingMode RawDVIInstructionsMode)) (progDesc ""))
   <> command "dvi-write" (info (ExpandingMode . DVIWriteMode <$> dviRunParser) (progDesc "Write DVI to a file"))
    )

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
  <$> runModeParser
  <*> inputParser
  <*> many (strOption (  long "dir" <> short 'd' <> metavar "SEARCH_DIR" <> help "Directory to search for support files"))
  <*> switch (long "amble" <> short 'a' <> help "Surround input with pre- and post-amble")

appOptionsParserInfo :: ParserInfo AppOptions
appOptionsParserInfo = info (appOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Run Hex source"
     <> header "Hex")

preamble :: BS.L.ByteString
preamble = "\\font\\thefont=cmr10 \\thefont\n\n"

postamble :: BS.L.ByteString
postamble = "\n\n\\end\n"

main :: IO ()
main = do
  opts <- execParser appOptionsParserInfo

  case mode opts of
    NonExpandingMode m -> case m of
      CatMode -> do
        foo <- runResourceT $ do
          src <- case input opts of
            StdInput ->
              pure stdin
            FileInput inPathStr -> do
              path <- Path.IO.resolveFile' (toS inPathStr)
              pure $ sourceFile (Path.toFilePath path)

          runConduit $ src .| usableExtractCharCat .| take 10 .| sinkList

        putText $ Tx.intercalate "\n" $ show <$> foo
        -- putText $ show (length foo)
      LexMode ->
        undefined
        -- putText $ Tx.intercalate "\n" $ show <$> usableCodesToLexTokens input
      ResolveMode ->
        undefined
        -- putText $ Tx.intercalate "\n" $ show <$> usableCodesToResolvedTokens input
    ExpandingMode m -> do
      (inputRaw, maybeInPath) <-
        case input opts of
          StdInput -> do
            cs <- liftIO BS.L.getContents
            pure (cs, Nothing)
          FileInput inPathStr -> do
            path <- Path.IO.resolveFile' (toS inPathStr)
            cs <- BS.L.readFile (Path.toFilePath path)
            pure (cs, Just path)
      let input =
            if withAmbles opts
              then preamble <> inputRaw <> postamble
              else inputRaw


      conf <- newConfig (maybeToList (Path.toFilePath . Path.parent <$> maybeInPath) <> (searchDirs opts))
      let inputS = newExpandStream maybeInPath input
      case m of
        ExpandMode -> do
          (endS, mayErr, primToks) <- App.runErrorlessApp (Run.expandingStreamAsPrimTokens inputS) conf
          case primToks of
            [] ->
              putText "Returned no results"
            _ ->
              putText $ resultTxt $
                renderLines $ describeNamedRelFoldable 0 "PrimitiveTokens" primToks

          for_ mayErr $ \err -> putText $ errTxtWithStream err endS
        CommandMode -> do
          (endS, mayErr, commands) <- App.runErrorlessApp (Run.expandingStreamAsCommands inputS) conf
          case commands of
            [] ->
              putText "Returned no results"
            _ ->
              putText $ resultTxt $
                renderLines $ describeNamedRelFoldable 0 "Commands" commands

          for_ mayErr $ \err -> putText $ errTxtWithStream err endS
        ParaListMode ->
          App.runApp (Run.renderStreamParaList inputS) conf >>= \case
            Left err ->
              putText $ errTxt err
            Right (_, txt) ->
              putText $ resultTxt txt
        ParaSetMode ->
          App.runApp (Run.renderStreamSetPara inputS) conf >>= \case
            Left err ->
              putText $ errTxt err
            Right (_, txt) ->
              putText $ resultTxt txt
        PageListMode ->
          App.runApp (Run.renderStreamPageList inputS) conf >>= \case
            Left err ->
              putText $ errTxt err
            Right (_, txt) ->
              putText $ resultTxt txt
        PageMode ->
          App.runApp (Run.renderStreamPages inputS) conf >>= \case
            Left err ->
              putText $ errTxt err
            Right (_, txt) ->
              putText $ resultTxt txt
        SemanticDVIInstructionsMode ->
          App.runApp (Run.renderStreamSemanticDVI inputS) conf >>= \case
            Left err ->
              putText $ errTxt err
            Right (_, txt) ->
              putText $ resultTxt txt
        RawDVIInstructionsMode ->
          App.runApp (Run.renderStreamRawDVI inputS) conf >>= \case
            Left err ->
              putText $ errTxt err
            Right (_, txt) ->
              putText $ resultTxt txt
        DVIWriteMode DVIWriteOptions { dviOutputPath } -> do
          let s = newExpandStream maybeInPath input
          App.runApp (Run.streamToDVIBytes s) conf >>= \case
            Left appError ->
              putText $ show appError
            Right (_endS, bytes) -> do
              putText $ "Writing to " <> toS dviOutputPath <> "..."
              liftIO $ BS.writeFile dviOutputPath bytes

errTxtWithStream :: App.AppError -> ExpandingStream -> Text
errTxtWithStream err s =
  errTxt err
  <> "\nStream ended in state:\n"
  <> renderDescribed s

errTxt :: App.AppError -> Text
errTxt err = "Ended with error:\n" <> show err

resultTxt :: Text -> Text
resultTxt r =
  "Got result:\n\n" <> r
