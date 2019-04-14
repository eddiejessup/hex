module HeX.BuildHelp where

import           HeXlude

import qualified Text.Megaparsec               as PS
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
import           Data.Either.Combinators        ( mapLeft )

import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Parse                     as HP

data BuildError s
  = ParseError s
  | ConfigError Text

type ER s a = StateT s IO a

newtype VM s a = VM { unVM :: ER s a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO)

type ExceptBuildT s m a = ExceptT (BuildError (HP.ParseErrorBundle s)) m a

type ExceptBuildVM s a = ExceptBuildT s (VM s) a

liftConfigError :: Monad m => ExceptT Text m a -> ExceptBuildT s m a
liftConfigError = withExceptT ConfigError

liftConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => StateT Config (ExceptT Text m) a
    -> ExceptBuildT s m a
liftConfState x = liftConfigError $ HP.runConfState x

liftReadOnConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => ReaderT Config (StateT Config (ExceptT Text m)) a
    -> ExceptBuildT s m a
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
    :: (HP.InhibitableStream s, MonadState s m, TeXEvaluable v)
    => v -> ExceptBuildT s m (EvalTarget v)
liftEvalOnConfState v = liftReadOnConfState $ texEvaluate v

extractList
    :: HP.InhibitableStream s
    => (s -> HP.ExtractResult s t)
    -> (s -> a -> t -> ExceptBuildVM s (a, Bool))
    -> a
    -> ExceptBuildVM s a
extractList extract process acc =
    do
    oldStream <- get
    com <- sth extract
    (procAcc, continue) <- process oldStream acc com
    if continue
        then extractList extract process procAcc
        else pure procAcc

sth
    :: (HP.InhibitableStream s, MonadState s m)
    => (s -> HP.ExtractResult s t)
    -> ExceptBuildT s m t
sth extract =
    do
    (PS.State { PS.stateInput = newStream }, v) <-
        gets extract
        <&> (ParseError `mapLeft`)
        >>= liftEither
    put newStream
    pure v
