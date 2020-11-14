{-# LANGUAGE DerivingVia #-}
module Hex.App where

import DVI.Instruction (DVIError)
import Data.Byte (ByteError)
import Data.Path (IOInputT(..), PathError)
import Hex.Build.Class (BuildError)
import qualified Hex.Config as Conf
import Hex.Evaluate (EvaluationError)
import Hexlude
import TFM (TFMError)
import qualified Hex.Parse.Stream.Class as S
import qualified Hex.Parse.Stream.Parse ()
import qualified Hex.Resolve.Resolve as R
import qualified Hex.Parse.TokenParser.Class as P
import Hex.Parse.TokenParser.ParseT (runTeXParseT, TeXParseT)
import Hex.Parse.Stream.Expanding (ExpandingStream)
import Hex.Lex (LexError)

data AppError
  = BuildAppError BuildError
  | ConfigAppError Conf.ConfigError
  | EvaluationAppError EvaluationError
  | PathAppError PathError
  | ExpansionAppError S.ExpansionError
  | ResolutionAppError R.ResolutionError
  | TFMAppError TFMError
  | ParseAppError P.ParseError
  | ByteAppError ByteError
  | DVIAppError DVIError
  | LexAppError LexError
  deriving stock (Show, Generic)

newtype App a
  = App {unApp :: TeXParseT ExpandingStream (IOInputT (ExceptT AppError (StateT Conf.Config IO))) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Conf.Config
    , MonadIO
    , MonadError AppError
    , Alternative
    , MonadPlus
    )

-- instance MonadSlog App where
--   sLog msg = do
--     loggerSet <- gets Conf.internalLoggerSet
--     liftIO $ Log.pushLogStrLn loggerSet (Log.toLogStr msg)

--   sTime = liftIO getCurrentTime

runApp
  :: MonadIO m
  => App a
  -> Conf.Config
  -> ExpandingStream
  -> m (Either AppError (ExpandingStream, Either P.ParseError a))
runApp a c s =
  runTexParseTApp (unApp a) c s

runTexParseTApp
  :: MonadIO m
  => TeXParseT ExpandingStream (IOInputT (ExceptT AppError (StateT Conf.Config IO))) a
  -> Conf.Config
  -> ExpandingStream
  -> m (Either AppError (ExpandingStream, Either P.ParseError a))
runTexParseTApp texParseT c s =
  let inputT = runTeXParseT texParseT s
  in runInputTApp inputT c

runInputTApp
  :: MonadIO m
  => IOInputT (ExceptT AppError (StateT Conf.Config IO)) a
  -> Conf.Config
  -> m (Either AppError a)
runInputTApp inputT c =
  let exceptT = unIOInputT inputT
      stateT = runExceptT exceptT
      io = liftIO $ evalStateT stateT c
  in io
