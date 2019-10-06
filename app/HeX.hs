module Main where

import           HeXlude

import           Control.Monad             (when)
import qualified Data.ByteString           as BS
import qualified System.Console.GetOpt     as Opt

import qualified Data.Sequence             as Seq
import qualified HeX.Config.Codes          as Code
import           HeX.Command.Run
import           HeX.Parse.Stream.Instance (newExpandStream)
import qualified Path.IO

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
    (inputRaw, maybePath) <- case args of
        ["-"] ->
            do
            cs <- fmap Code.CharCode . Seq.fromList . BS.unpack <$> liftIO BS.getContents
            pure (cs, Nothing)
        [pathStr] -> do
            path <- Path.IO.resolveFile' (toS pathStr)
            cs <- Code.readCharCodes path
            pure (cs, Just path)
    let
        input = if Amble `elem` flags
            then preamble <> inputRaw <> postamble
            else inputRaw
        mode = lastDef DVIWriteMode [ m | (Mode m) <- flags ]
        destPathStr = lastDef "out.dvi" [ f | (Output f) <- flags ]
        makeStream = newExpandStream maybePath input
    case mode of
        CatMode      -> runCat input
        LexMode      -> runLex input
        ResolveMode  -> runResolved input
        ExpandMode   -> makeStream >>= runExpand
        CommandMode  -> makeStream >>= runCommand
        ParaListMode -> makeStream >>= runPara
        ParaSetMode  -> makeStream >>= runSetPara
        PageMode     -> makeStream >>= runPages
        DVIMode      -> makeStream >>= runDVI
        RawDVIMode   -> makeStream >>= runDVIRaw
        DVIWriteMode -> makeStream >>= codesToDVIBytes >>= BS.writeFile destPathStr
