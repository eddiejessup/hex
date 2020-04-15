{-# LANGUAGE RankNTypes #-}

module Hex.Command.Common where

import           Hexlude

import           Hex.Config
import           Hex.Evaluate
import           Hex.Quantity
import qualified Hex.Parse                 as HP

readOnState
    :: ( MonadState r m
       )
     => ReaderT r m b
     -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: ( HP.TeXStream s
       , MonadState s m
       )
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

modConfState
    :: (MonadState s m, HP.TeXStream s) => (Config -> Config) -> m ()
modConfState x = HP.runConfState $ modify x

evalOnConfState
    :: ( HP.TeXStream s
       , TeXEvaluable v
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => v -> m (EvalTarget v)
evalOnConfState v = readOnConfState $ texEvaluate v

data BoxModeIntent
    = IntentToAddBox
    | IntentToSetBoxRegister EightBitInt HP.GlobalFlag
    deriving stock (Show)


data RecursionResult a b
    = LoopAgain a
    | EndLoop b

addMaybeElem :: Seq a -> Maybe a -> Seq a
addMaybeElem a mayE = case mayE of
    Nothing -> a
    Just e -> a :|> e

runLoop :: Monad m => (a -> m (RecursionResult a b)) -> a -> m b
runLoop f = go
  where
    go state_ =
        f state_ >>= \case
            LoopAgain newState ->
                go newState
            EndLoop result ->
                pure result

runCommandLoop
    :: ( HP.TeXParseable s e m
       , MonadState s m
       )
    => (st -> HP.Command -> s -> m (RecursionResult st r))
    -> st
    -> m r
runCommandLoop f = runLoop g
  where
    g elemList =
        do
        oldStream <- get
        (newStream, command) <- HP.runSimpleRunParserT' HP.parseCommand oldStream
        put newStream
        -- liftIO $ putText $ describe command
        f elemList command oldStream
