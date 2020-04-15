module Main where

-- import Control.Monad (when)
-- import qualified Data.ByteString.Lazy as BS.L
-- import qualified Data.Sequence as Seq
-- import Hex.Command.Run
-- import qualified Hex.Config.Codes as Code
-- import Hex.Parse (newExpandStream)
-- import Hexlude
-- import qualified Path
-- import qualified Path.IO
-- import qualified System.Console.GetOpt as Opt

-- data Mode
--   = CatMode
--   | LexMode
--   | ResolveMode
--   | ExpandMode
--   | CommandMode
--   | ParaListMode
--   | ParaSetMode
--   | PageListMode
--   | PageMode
--   | DVIMode
--   | RawDVIMode
--   | DVIWriteMode
--   deriving (Show, Eq)

-- data Flag
--   = Help
--   | Amble
--   | Output !FilePath
--   | Mode !Mode
--   deriving (Show, Eq)

-- options :: [Opt.OptDescr Flag]
-- options =
--   [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "show usage information"
--   , Opt.Option ['a'] ["amble"] (Opt.NoArg Amble) "prepend pre- and post-amble to input"
--   , Opt.Option ['o'] ["output"] (Opt.OptArg output "FILE") "output to FILE"
--   , Opt.Option ['m'] ["mode"] (Opt.OptArg mode "MODE") "output in mode MODE"
--   ]
--   where
--     output = Output . toS . fromMaybe "out.dvi"
--     mode m =
--       Mode $ case toS <$> m of
--         Nothing -> DVIWriteMode
--         Just "cat" -> CatMode
--         Just "lex" -> LexMode
--         Just "resolve" -> ResolveMode
--         Just "expand" -> ExpandMode
--         Just "command" -> CommandMode
--         Just "paralist" -> ParaListMode
--         Just "paraset" -> ParaSetMode
--         Just "pagelist" -> PageListMode
--         Just "page" -> PageMode
--         Just "dvi" -> DVIMode
--         Just "rawdvi" -> RawDVIMode
--         Just "bytes" -> DVIWriteMode
--         Just s -> panic $ "Unknown mode: " <> s

-- usage :: Text
-- usage = toS $ Opt.usageInfo header options
--   where
--     header = "Usage: hex [OPTION...] [file]"

-- preamble :: Seq Code.CharCode
-- preamble = Seq.fromList $ Code.codesFromStr "\\font\\thefont=cmr10 \\thefont\n\n"

-- postamble :: Seq Code.CharCode
-- postamble = Seq.fromList $ Code.codesFromStr "\n\n\\end\n"

-- parseArgs :: [Text] -> IO ([Flag], [Text])
-- parseArgs argStr = case Opt.getOpt Opt.Permute options (toS <$> argStr) of
--   (o, n, []) -> pure (o, toS <$> n)
--   (_, _, errs) -> panic ((mconcat (toS <$> errs) <> usage) :: Text)

-- main :: IO ()
-- main = do
--   (flags, args) <- ((toS <$>) <$> getArgs) >>= parseArgs
--   when (Help `elem` flags) $ panic usage
--   inputRaw <-
--     case args of
--       ["-"] ->
--         liftIO BS.L.getContents
--       [pathStr] -> do
--         path <- Path.IO.resolveFile' (toS pathStr)
--         BS.L.readFile (Path.toFilePath path)
--       _ ->
--         panic "Please specify a file to execute, or '-' to read from the standard-input stream"
--   -- runResolved inputRaw

--   -- conf0 <- Conf.newConfig
--   -- let setCS name confIn =
--   --         Conf.setControlSequence
--   --             (Lex.ControlSequenceProper $ Lex.mkControlSequence name)
--   --             (Parse.PrimitiveToken Parse.RelaxTok)
--   --             Parse.Local
--   --             confIn
--   -- let conf1 = setCS (toS ("notpar" :: [Char])) conf0
--   -- let conf2 = doN (setCS (toS ("par" :: [Char]))) 100000 conf1
--   -- seq conf2 (pure ())
--   stream <- newExpandStream Nothing inputRaw
--   runPageList stream

-- -- benchFetchLex stream
-- -- benchTake stream
-- -- runCommand stream
-- doN :: (Eq a, Num a, Enum a) => (b -> b) -> a -> b -> b
-- doN f = go
--   where
--     go n v
--       | n == 0 = v
--       | otherwise = let nv = f v in seq nv (go (pred n) (f v))

-- -- benchTake
-- --     :: HP.ExpandingStream
-- --     -> IO ()
-- -- benchTake s = runExceptT (P.take1_ s) >>= \case
-- --     Left (err :: AppError) -> panic $ show err
-- --     Right Nothing -> pure ()
-- --     Right (Just (_, s1)) -> benchTake s1

-- -- benchFetchLex
-- --     :: HP.ExpandingStream
-- --     -> IO ()
-- -- benchFetchLex s = case HP.fetchLexToken s of
-- --     Nothing -> pure ()
-- --     Just (_, s1) -> benchFetchLex s1
-- -- benchLex
-- --     :: ( MonadIO m
-- --        )
-- --     => BS.L.ByteString
-- --     -> m ()
-- -- benchLex = go LineBegin
-- --   where
-- --     go lexState xs =
-- --         case extractToken usableCatLookup lexState xs of
-- --             Just (_, lexState1, xs1) ->
-- --                 go lexState1 xs1
-- --             Nothing ->
-- --                 pure ()

main = pure ()
