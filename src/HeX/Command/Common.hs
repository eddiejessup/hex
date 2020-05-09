module Hex.Command.Common where

import qualified Hex.Parse as HP
import Hexlude
import Hex.Config.Config

addMaybeElem :: Seq a -> Maybe a -> Seq a
addMaybeElem a = \case
  Nothing -> a
  Just e -> a :|> e

runLoop :: Monad m => (s -> a -> m (s, a, Maybe b)) -> s -> a -> m (s, a, b)
runLoop f = go
  where
    go s state_ = do
      (newS, newState, recRes) <- f s state_
      case recRes of
        Nothing ->
          go newS newState
        Just b ->
          pure (newS, newState, b)

runCommandLoop
  :: ( MonadState st m
     , HasType Config st

     , HP.MonadTeXParse (HP.TeXParseT s m)

     , MonadError e m
     , HP.AsTeXParseErrors e
     , AsType HP.ParseError e

     , MonadSlog m
     )
  => (s -> s -> a -> HP.Command -> m (s, a, Maybe b))
  -> s
  -> a
  -> m (s, a, b)
runCommandLoop runCommand = runLoop parseAndRunCommand
  where
    parseAndRunCommand oldS elemList = do
      (newS, command) <- HP.runTeXParseTEmbedded HP.parseCommand oldS
      sLog $ "In parseAndRunCommand, got command " <> renderDescribed command
      runCommand oldS newS elemList command
