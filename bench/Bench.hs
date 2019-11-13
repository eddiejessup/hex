module Main where

import           HeXlude

import           Control.Monad             (when)
import qualified Data.ByteString.Lazy      as BS.L
import qualified System.Console.GetOpt     as Opt

import qualified Data.Sequence             as Seq
import qualified HeX.Config.Codes          as Code
import           HeX.Command.Run
import           HeX.Parse                 (newExpandStream)
import qualified Path
import qualified Path.IO

data Mode
    = CatMode
    | LexMode
    | ResolveMode
    | ExpandMode
    | CommandMode
    | ParaListMode
    | ParaSetMode
    | PageListMode
    | PageMode
    | DVIMode
    | RawDVIMode
    | DVIWriteMode
    deriving (Show, Eq)

data Flag
    = Help
    | Amble
    | Output !FilePath
    | Mode !Mode
    deriving (Show, Eq)

options :: [Opt.OptDescr Flag]
options =
    [ Opt.Option ['h'] ["help"]   (Opt.NoArg Help)           "show usage information"
    , Opt.Option ['a'] ["amble"]  (Opt.NoArg Amble)          "prepend pre- and post-amble to input"
    , Opt.Option ['o'] ["output"] (Opt.OptArg output "FILE") "output to FILE"
    , Opt.Option ['m'] ["mode"]   (Opt.OptArg mode "MODE")   "output in mode MODE"
    ]
  where
    output = Output . toS . fromMaybe "out.dvi"

    mode m = Mode $ case toS <$> m of
        Nothing         -> DVIWriteMode
        Just "cat"      -> CatMode
        Just "lex"      -> LexMode
        Just "resolve"  -> ResolveMode
        Just "expand"   -> ExpandMode
        Just "command"  -> CommandMode
        Just "paralist" -> ParaListMode
        Just "paraset"  -> ParaSetMode
        Just "pagelist" -> PageListMode
        Just "page"     -> PageMode
        Just "dvi"      -> DVIMode
        Just "rawdvi"   -> RawDVIMode
        Just "bytes"    -> DVIWriteMode
        Just s          -> panic $ "Unknown mode: " <> s

usage :: Text
usage = toS $ Opt.usageInfo header options
  where
    header = "Usage: hex [OPTION...] [file]"

preamble :: Seq Code.CharCode
preamble = Seq.fromList $ Code.codesFromStr "\\font\\thefont=cmr10 \\thefont\n\n"

postamble :: Seq Code.CharCode
postamble = Seq.fromList $ Code.codesFromStr "\n\n\\end\n"

parseArgs :: [Text] -> IO ([Flag], [Text])
parseArgs argStr =
    case Opt.getOpt Opt.Permute options (toS <$> argStr) of
        (o, n, []  ) -> pure (o, toS <$> n)
        (_, _, errs) -> panic ((mconcat (toS <$> errs) <> usage) :: Text)

main :: IO ()
main = do
    (flags, args) <- ((toS <$>) <$> getArgs) >>= parseArgs
    when (Help `elem` flags) $ panic usage

    inputRaw <- case args of
        ["-"] ->
            liftIO BS.L.getContents
        [pathStr] ->
            do
            path <- Path.IO.resolveFile' (toS pathStr)
            BS.L.readFile (Path.toFilePath path)
        _ ->
            panic "Please specify a file to execute, or '-' to read from the standard-input stream"

    -- runResolved inputRaw

    stream <- newExpandStream Nothing inputRaw

    -- benchFetchLex stream
    -- benchTake stream
    -- runCommand stream

    -- runPageList stream
    runPageList stream
