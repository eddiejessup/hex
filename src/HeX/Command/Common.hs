module HeX.Command.Common where

import           HeXlude

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , liftEither
                                                , throwError
                                                , withExceptT
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT
                                                , get
                                                , modify
                                                )
import           Data.Either.Combinators     (mapLeft)
import qualified Text.Megaparsec             as P

import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Parse                     as HP

data BuildError s
  = ParseError s
  | ConfigError Text

newtype MonadBuild s a = MonadBuild { unMonadBuild :: StateT s IO a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO)

type BaseExceptMonadBuild e s a = ExceptT e (MonadBuild s) a

type ExceptMonadBuild s a = ExceptT (BuildError (HP.ParseErrorBundle s)) (MonadBuild s) a

liftConfigError :: BaseExceptMonadBuild Text s a -> ExceptMonadBuild s a
liftConfigError = withExceptT ConfigError

liftConfState
    :: HP.InhibitableStream s
    => StateT Config (ExceptT Text (MonadBuild s)) a
    -> ExceptMonadBuild s a
liftConfState x = liftConfigError $ HP.runConfState x

liftReadOnConfState
    :: HP.InhibitableStream s
    => ReaderT Config (StateT Config (ExceptT Text (MonadBuild s))) a
    -> ExceptMonadBuild s a
liftReadOnConfState x = liftConfigError $ readOnConfState x

throwConfigError :: MonadError (BuildError s) m => Text -> m a
throwConfigError s = throwError $ ConfigError s

liftMaybeConfigError :: MonadError (BuildError s) m => Text -> Maybe a -> m a
liftMaybeConfigError s = liftMaybe (ConfigError s)

readOnState :: MonadState r m => ReaderT r m b -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

modConfState
    :: (MonadState s m, HP.InhibitableStream s) => (Config -> Config) -> m ()
modConfState x = HP.runConfState $ modify $ x

liftEvalOnConfState
    :: (HP.InhibitableStream s, TeXEvaluable v)
    => v -> ExceptMonadBuild s (EvalTarget v)
liftEvalOnConfState v = liftReadOnConfState $ texEvaluate v

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
runLoop f initialState = go initialState
  where
    go state_ =
        f state_ >>= \case
            LoopAgain newState ->
                go newState
            EndLoop result ->
                pure result

runCommandLoop
    :: HP.InhibitableStream s
    => (st -> HP.Command -> s -> ExceptMonadBuild s (RecursionResult st r))
    -> st
    -> ExceptMonadBuild s r
runCommandLoop f s = runLoop g s
  where
    g elemList =
        do
        (oldStream, command) <- peekCommand
        f elemList command oldStream

    peekCommand :: HP.InhibitableStream s => ExceptMonadBuild s (s, HP.Command)
    peekCommand =
        do
        oldStream <- get
        (P.State { P.stateInput = newStream }, command) <- liftEither $ ParseError `mapLeft` HP.extractCommand oldStream
        put newStream
        pure (oldStream, command)
