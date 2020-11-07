module Main where

import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Hex.App as App
import Hex.Build.ListBuilderT
import Hex.Categorise
import Hex.Config
import Hex.Lex
import Hex.Parse
import Hex.Resolve
import Hex.Run
import Hexlude
import System.IO (BufferMode (..), hSetBuffering)
import qualified Data.Text as Tx

data Flag
  = Help
  deriving stock (Show, Eq)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putText "Welcome to Hexi, the Hex REPL"
  let s = newExpandStream Nothing mempty
  conf <- newConfig []
  evalStateT (forever repl) (s, conf)

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

repl :: (MonadState (ExpandingStream, Config) m, MonadIO m) => m ()
repl = do
  s <- use (typed @ExpandingStream)
  conf <- use (typed @Config)
  inp <- prompt ""
  let (cmd, arg) = Tx.breakOn " " inp
  let argBSL = BS.L.fromStrict $ encodeUtf8 arg
  case cmd of
    "cat" -> do
      let charCats = usableCodesToCharCats argBSL
      putResult $ renderLines $ describeNamedRelFoldable 0 "CharCats" charCats
    "lex" -> do
      let lexTokens = usableCodesToLexTokens argBSL
      putResult $ renderLines $ describeNamedRelFoldable 0 "LexTokens" lexTokens
    "resolve" -> do
      let lexWithResolvedTokens = usableCodesToResolvedTokens argBSL
      putResult $ renderLines $ concat $
        lexWithResolvedTokens <&> \(lt, mayRt) ->
          [ (0, "Lex with resolved token")
          ]
            <> describeRel 1 lt
            <> describeRel 1 mayRt
    "expand" -> do
      let sWithInput =
            s & G.P.field @"streamTokenSources"
              %~ L.NE.cons (newTokenSource Nothing argBSL)

      -- TODO: Put back into state? What about version with input but before consuming?
      (newS, mayErr, primToks) <- App.runErrorlessApp (expandingStreamAsPrimTokens sWithInput) conf

      putResult $ renderLines $ describeNamedRelFoldable 0 "PrimitiveTokens" primToks
      putMayError newS mayErr
    "command" -> do
      let sWithInput =
            s & G.P.field @"streamTokenSources"
              %~ L.NE.cons (newTokenSource Nothing argBSL)

      (newS, mayErr, commandToks) <- App.runErrorlessApp (expandingStreamAsCommands sWithInput) conf

      putResult $ renderLines $ describeNamedRelFoldable 0 "Commands" commandToks
      putMayError newS mayErr
    "paralist" -> do
      let sWithInput =
            s & G.P.field @"streamTokenSources"
              %~ L.NE.cons (newTokenSource Nothing argBSL)

      App.runApp (extractParaListImpl DoNotIndent sWithInput) conf >>= \case
        Right (_, hList, endParaReason) -> do
          putText "Ended because:"
          putText $ show endParaReason
          putText "\n"
          putResult $ renderDescribed hList
        Left err -> do
          putText "Ended with error:"
          putText $ show err
