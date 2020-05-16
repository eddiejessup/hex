module Main where

import           Control.Monad.Catch      (MonadThrow)
import qualified Data.ByteString.Lazy as BS.L
import Hex.Categorise
import Hex.Command.Run
import Hex.App
import Hex.Lex
import Hex.Resolve
import Hex.Parse
import Hexlude
import qualified System.Console.GetOpt as Opt
import System.IO (hSetBuffering, BufferMode(..))
import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Hex.Config as Conf

data Flag
  = Help
  deriving stock (Show, Eq)

options :: [Opt.OptDescr Flag]
options =
  [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "show usage information"
  ]

usage :: Text
usage = toS $ Opt.usageInfo header options
  where
    header = "Usage: hexrepl [OPTION...] [file]"

parseArgs :: [Text] -> IO ([Flag], [Text])
parseArgs argStr = case Opt.getOpt Opt.Permute options (toS <$> argStr) of
  (o, n, []) -> pure (o, toS <$> n)
  (_, _, errs) -> panic ((mconcat (toS <$> errs) <> usage) :: Text)

main :: IO ()
main = do
  (flags, _) <- ((toS <$>) <$> getArgs) >>= parseArgs
  when (Help `elem` flags) $ panic usage
  hSetBuffering stdout NoBuffering
  putText "Welcome to Hex repl"
  let s = newExpandStream Nothing mempty
  evalStateT repl s

putResult :: MonadIO m => Text -> m ()
putResult r =
  putText $ "Got result:\n\n" <> r

prompt :: MonadIO m => Text -> m Text
prompt msg = do
  liftIO $ putStr @Text (msg <> " $> ")
  liftIO getLine

promptLazyByteString :: MonadIO m => Text -> m BS.L.ByteString
promptLazyByteString msg = do
  liftIO $ putStr @Text (msg <> " $> ")
  liftIO $ BS.L.fromStrict . encodeUtf8 <$> getLine

repl :: (MonadState ExpandingStream m, MonadIO m, MonadThrow m) => m ()
repl = do
  cmd <- prompt "Enter command, one of: [cat, lex, resolve, expand, command]"
  conf <- Conf.newConfig []
  case cmd of
    "cat" -> do
      inpBSL <- promptLazyByteString "cat"
      let charCats = usableCodesToCharCats inpBSL
      putResult $ renderLines $ describeNamedRelFoldable 0 "CharCats" charCats
    "lex" -> do
      inpBSL <- promptLazyByteString "lex"
      let lexTokens = usableCodesToLexTokens inpBSL
      putResult $ renderLines $ describeNamedRelFoldable 0 "LexTokens" lexTokens
    "resolve" -> do
      inpBSL <- promptLazyByteString "resolve"
      let lexWithResolvedTokens = usableCodesToResolvedTokens inpBSL
      putResult $ renderLines $ concat $ lexWithResolvedTokens <&> \(lt, mayRt) ->
        [ (0, "Lex with resolved token")
        ]
        <> describeRel 1 lt
        <> describeRel 1 mayRt
    "expand" -> do
      inpBSL <- promptLazyByteString "expand"

      s <- get

      let sWithInput = s & G.P.field @"streamTokenSources" %~
              L.NE.cons (newTokenSource Nothing inpBSL)

      -- TODO: Put back into state? What about version with input but before consuming?
      (newS, mayErr, primToks) <- expandingStreamAsPrimTokens sWithInput conf

      case primToks of
        [] -> putText "Returned no results"
        _ -> putResult $ renderLines $ describeNamedRelFoldable 0 "PrimitiveTokens" primToks

      for_ mayErr $ \(err :: AppError) -> do
        putText "Ended with error:"
        putText $ show err
        putText "Stream ended in state:"
        putText $ renderDescribed newS

    "command" -> do
      inpBSL <- promptLazyByteString "command"

      s <- get

      let sWithInput = s & G.P.field @"streamTokenSources" %~
              L.NE.cons (newTokenSource Nothing inpBSL)

      (newS, mayErr, commandToks) <- expandingStreamAsCommands sWithInput conf

      case commandToks of
        [] -> putText "Returned no results"
        _ -> putResult $ renderLines $ describeNamedRelFoldable 0 "Commands" commandToks

      for_ mayErr $ \case
        ParseAppError EndOfInput ->
          pure ()
        err -> do
          putText "Ended with error:"
          putText $ show err
          putText "Stream ended in state:"
          putText $ renderDescribed newS
    _ ->
      putText $ "Unrecognised command: " <> cmd
  repl

