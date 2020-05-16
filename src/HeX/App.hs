module Hex.App where

import DVI.Instruction (DVIError)
import Data.Byte (ByteError)
import Data.Path (PathError)
import Hex.Command.Build
import qualified Hex.Config as Conf
import Hex.Evaluate (EvaluationError)
import qualified Hex.Parse as HP
import Hexlude
import TFM (TFMError)
import qualified System.Log.FastLogger as Log
import Data.Time.Clock

data AppError
  = BuildAppError BuildError
  | ConfigAppError Conf.ConfigError
  | EvaluationAppError EvaluationError
  | PathAppError PathError
  | ExpansionAppError HP.ExpansionError
  | ResolutionAppError HP.ResolutionError
  | TFMAppError TFMError
  | ParseAppError HP.ParseError
  | ByteAppError ByteError
  | DVIAppError DVIError
  deriving stock (Show, Generic)

newtype App a
  = App {unApp :: ExceptT AppError (StateT Conf.Config IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Conf.Config
    , MonadIO
    , MonadError AppError
    )

instance MonadSlog App where
  sLog msg = do
    loggerSet <- gets Conf.internalLoggerSet
    liftIO $ Log.pushLogStrLn loggerSet (Log.toLogStr msg)

  sTime = liftIO getCurrentTime

runApp
  :: MonadIO m
  => App a
  -> Conf.Config
  -> m (Either AppError a)
runApp f c =
  liftIO (evalStateT (runExceptT $ unApp f) c)



newtype ErrorlessApp a
  = ErrorlessApp {unErrorlessApp :: StateT Conf.Config IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Conf.Config
    , MonadIO
    )

instance MonadSlog ErrorlessApp where
  sLog msg = do
    loggerSet <- gets Conf.internalLoggerSet
    liftIO $ Log.pushLogStrLn loggerSet (Log.toLogStr msg)

  sTime = liftIO getCurrentTime

runErrorlessApp
  :: MonadIO m
  => ErrorlessApp a
  -> Conf.Config
  -> m a
runErrorlessApp f c =
  liftIO (evalStateT (unErrorlessApp f) c)
