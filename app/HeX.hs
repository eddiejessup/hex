module Main where

import qualified Prelude
import           Protolude
import           Protolude.Panic                ( panic )

import qualified Data.ByteString                as BS
import qualified System.Console.GetOpt          as Opt
import           Control.Monad                  ( when )

import           HeX.Categorise                 ( CharCode )
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
    | Output !FilePath
    | Mode !Mode
    deriving (Show, Eq)

options :: [Opt.OptDescr Flag]
options =
    [ Opt.Option ['h'] ["help"]   (Opt.NoArg Help)           "show usage information"
    , Opt.Option ['a'] ["amble"]  (Opt.NoArg Amble)          "prepend pre- and post-amble to input"
    , Opt.Option ['o'] ["output"] (Opt.OptArg output "FILE") "output to FILE"
    , Opt.Option ['m'] ["mode"]   (Opt.OptArg (mode) "MODE")   "output in mode MODE"
    ]
  where
    output = Output . toS . fromMaybe "out.dvi"

    mode m = Mode $ case (toS <$> m) of
        Nothing          -> DVIWriteMode
        Just "cat"       -> CatMode
        Just "lex"       -> LexMode
        Just "resolve"   -> ResolveMode
        Just "expand"    -> ExpandMode
        Just "command"   -> CommandMode
        Just "paralist"  -> ParaListMode
        Just "paraset"   -> ParaSetMode
        Just "page"      -> PageMode
        Just "dvi"       -> DVIMode
        Just "rawdvi"    -> RawDVIMode
        Just "bytes"     -> DVIWriteMode
        Just s           -> panic $ "Unknown mode: " <> s

usage :: Text
usage = toS $ Opt.usageInfo header options
  where
    header = "Usage: hex [OPTION...] [file]"

preamble, postamble :: [CharCode]
preamble = "\\font\\thefont=cmr10 \\thefont\n\n"
postamble = "\n\n\\end\n"

parseArgs :: [Text] -> IO ([Flag], [Text])
parseArgs argStr =
    case Opt.getOpt Opt.Permute options (toS <$> argStr) of
        (o, n, []  ) -> pure (o, toS <$> n)
        (_, _, errs) -> panic $ ((mconcat (toS <$> errs) <> usage) :: Text)

main :: IO ()
main = do
    (flags, args) <- ((toS <$>) <$> getArgs) >>= parseArgs
    when (Help `elem` flags) $ panic usage
    inputRaw <- case args of
        ["-"] -> Prelude.getContents
        [f  ] -> Prelude.readFile (toS f)
        _     -> panic usage
    let
        input = if Amble `elem` flags
            then preamble <> inputRaw <> postamble
            else inputRaw
        mode = lastDef DVIWriteMode [ m | (Mode m) <- flags ]
        dest = lastDef "out.dvi" [ f | (Output f) <- flags ]
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
        DVIWriteMode -> codesToDVIBytes input >>= BS.writeFile dest
