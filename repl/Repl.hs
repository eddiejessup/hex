module Main where

import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Hex.App as App
import Hex.Categorise
import qualified Hex.Config as Conf
import Hex.Lex
import Hex.Parse
import Hex.Resolve
import Hex.Run
import Hexlude
import qualified System.Console.GetOpt as Opt
import System.IO (BufferMode (..), hSetBuffering)

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
  conf <- Conf.newConfig []
  evalStateT (forever repl) (DoCat, s, conf)

putResult :: MonadIO m => Text -> m ()
putResult r =
  putText $ ">>>\n" <> r <> "\n<<<"

putMayError :: (MonadIO m, Describe a) => a -> Maybe App.AppError -> m ()
putMayError s = \case
  Nothing ->
    pure ()
  Just (App.ParseAppError EndOfInput) ->
    pure ()
  Just (err :: App.AppError) -> do
    putText "Ended with error:"
    putText $ show err
    putText "Stream ended in state:"
    putText $ renderDescribed s

prompt :: MonadIO m => Text -> m Text
prompt msg = do
  liftIO $ putStr @Text (msg <> " $> ")
  liftIO getLine

data ReplState
  = DoCat
  | DoLex
  | DoResolve
  | DoExpand
  | DoCommand

renderReplState :: ReplState -> Text
renderReplState = \case
  DoCat -> "cat"
  DoLex -> "lex"
  DoResolve -> "resolve"
  DoExpand -> "expand"
  DoCommand -> "command"

repl :: (MonadState (ReplState, ExpandingStream, Conf.Config) m, MonadIO m) => m ()
repl = do
  replState <- gets $ view $ typed @ReplState
  s <- gets (view (typed @ExpandingStream))
  conf <- gets (view (typed @Conf.Config))
  cmd <- prompt $ "[" <> renderReplState replState <> "]"
  case cmd of
    "$cat" ->
      modify $ typed @ReplState .~ DoCat
    "$lex" ->
      modify $ typed @ReplState .~ DoLex
    "$resolve" ->
      modify $ typed @ReplState .~ DoResolve
    "$expand" ->
      modify $ typed @ReplState .~ DoExpand
    "$command" ->
      modify $ typed @ReplState .~ DoCommand
    _ -> do
      let inpBSL = BS.L.fromStrict $ encodeUtf8 cmd
      case replState of
        DoCat -> do
          let charCats = usableCodesToCharCats inpBSL
          putResult $ renderLines $ describeNamedRelFoldable 0 "CharCats" charCats
        DoLex -> do
          let lexTokens = usableCodesToLexTokens inpBSL
          putResult $ renderLines $ describeNamedRelFoldable 0 "LexTokens" lexTokens
        DoResolve -> do
          let lexWithResolvedTokens = usableCodesToResolvedTokens inpBSL
          putResult $ renderLines $ concat $
            lexWithResolvedTokens <&> \(lt, mayRt) ->
              [ (0, "Lex with resolved token")
              ]
                <> describeRel 1 lt
                <> describeRel 1 mayRt
        DoExpand -> do
          let sWithInput =
                s & G.P.field @"streamTokenSources"
                  %~ L.NE.cons (newTokenSource Nothing inpBSL)

          -- TODO: Put back into state? What about version with input but before consuming?
          (newS, mayErr, primToks) <- App.runErrorlessApp (expandingStreamAsPrimTokens sWithInput) conf

          putResult $ renderLines $ describeNamedRelFoldable 0 "PrimitiveTokens" primToks
          putMayError newS mayErr
        DoCommand -> do
          let sWithInput =
                s & G.P.field @"streamTokenSources"
                  %~ L.NE.cons (newTokenSource Nothing inpBSL)

          (newS, mayErr, commandToks) <- App.runErrorlessApp (expandingStreamAsCommands sWithInput) conf

          putResult $ renderLines $ describeNamedRelFoldable 0 "Commands" commandToks
          putMayError newS mayErr
