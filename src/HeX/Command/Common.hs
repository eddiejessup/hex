module Hex.Command.Common where

import qualified Hex.Parse as HP
import Hexlude

addMaybeElem :: Seq a -> Maybe a -> Seq a
addMaybeElem a mayE = case mayE of
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
  :: ( HP.TeXParseable s st e m
     )
  => (s -> s -> a -> HP.Command -> m (s, a, Maybe b))
  -> s
  -> a
  -> m (s, a, b)
runCommandLoop runCommand = runLoop parseAndRunCommand
  where
    parseAndRunCommand oldS elemList = do
      (newS, command) <- HP.runSimpleRunParserT' HP.parseCommand oldS
      runCommand oldS newS elemList command
