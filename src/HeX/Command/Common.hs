{-# LANGUAGE RankNTypes #-}

module HeX.Command.Common where

import           HeXlude

import           Control.Monad.State.Lazy  (MonadState, StateT, get, modify)

import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Parse                 as HP

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
    deriving ( Show )


data RecursionResult a b
    = LoopAgain a
    | EndLoop b

doNothing :: a -> RecursionResult a b
doNothing = LoopAgain

addElem :: ForwardDirected Seq a -> a -> RecursionResult (ForwardDirected Seq a) b
addElem a e = LoopAgain (a ->. e)

addMaybeElem :: ForwardDirected Seq a -> Maybe a -> RecursionResult (ForwardDirected Seq a) b
addMaybeElem a = \case
    Nothing -> doNothing a
    Just e -> addElem a e

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
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m HP.TeXStreamE
       , MonadState s m
       , MonadIO m
       )
    => (st -> HP.Command -> s -> m (RecursionResult st r))
    -> st
    -> m r
runCommandLoop f = runLoop g
  where
    g elemList =
        do
        oldStream <- get
        (newStream, command) <- HP.runParser HP.parseCommand oldStream
        put newStream
        liftIO $ print command
        f elemList command oldStream
