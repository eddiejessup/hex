module Hex.Command.Common where

import qualified Hex.Parse as HP
import Hexlude

data RecursionResult a b
  = LoopAgain a
  | EndLoop b

addMaybeElem :: Seq a -> Maybe a -> Seq a
addMaybeElem a mayE = case mayE of
  Nothing -> a
  Just e -> a :|> e

runLoop :: Monad m => (s -> a -> m (s, RecursionResult a b)) -> s -> a -> m (s, b)
runLoop f = go
  where
    go s state_ = do
      (newS, recRes) <- f s state_
      case recRes of
        LoopAgain newState ->
          go newS newState
        EndLoop result ->
          pure (newS, result)

runCommandLoop
  :: ( HP.TeXParseable s st e m
     )
  => (s -> s -> a -> HP.Command -> m (s, RecursionResult a r))
  -> s
  -> a
  -> m (s, r)
runCommandLoop runCommand = runLoop parseAndRunCommand
  where
    parseAndRunCommand oldS elemList = do
      (newS, command) <- HP.runSimpleRunParserT' HP.parseCommand oldS
      runCommand oldS newS elemList command
