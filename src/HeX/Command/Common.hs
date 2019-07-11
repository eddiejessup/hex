{-# LANGUAGE RankNTypes #-}

module HeX.Command.Common where

import           HeXlude

import           Control.Monad.Except     (ExceptT)
import           Control.Monad.State.Lazy (MonadState, StateT, get, modify)
import           Control.Monad.Trans.Maybe (MaybeT (..))

import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Parse                as HP

newtype MonadBuild s a = MonadBuild { unMonadBuild :: ExceptT Text (StateT s IO) a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO, MonadError Text)

readOnState :: MonadState r m => ReaderT r m b -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: (HP.TeXStream s, MonadState s m)
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

modConfState
    :: (MonadState s m, HP.TeXStream s) => (Config -> Config) -> m ()
modConfState x = HP.runConfState $ modify x

evalOnConfState
    :: (HP.TeXStream s, TeXEvaluable v)
    => v -> MonadBuild s (EvalTarget v)
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
    :: HP.TeXStream s
    => (st -> HP.Command -> s -> MonadBuild s (RecursionResult st r))
    -> st
    -> MonadBuild s r
runCommandLoop f = runLoop g
  where
    g elemList =
        do
        oldStream <- get
        (newStream, command) <- liftIO (runExceptT (runMaybeT (HP.runParser HP.parseCommand oldStream))) >>= \case
            Left err -> throwError $ show err
            Right Nothing -> throwError "End of input"
            Right (Just v) -> pure v
        put newStream
        liftIO $ print command
        f elemList command oldStream
