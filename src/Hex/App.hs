module Hex.App where

import DVI.Instruction (DVIError)
import Data.Byte (ByteError)
import Data.Path (PathError)
import Hex.Build.Class (BuildError)
import qualified Hex.Config as Conf
import Hex.Evaluate (ConditionBodyState, EvaluationError)
import Hexlude
import TFM (TFMError)
import qualified System.Log.FastLogger as Log
import Data.Time.Clock
import qualified Hex.Parse.Stream.Class as S
import qualified Hex.Parse.Stream.Expanding.Parse ()
import qualified Hex.Resolve.Resolve as R
import qualified Hex.Parse.TokenParser.Class as P
import Hex.Parse.TokenParser.ParseT (runTeXParseTEmbedded, TeXParseT)
import Hex.Parse.Stream.Expanding (ExpandingStream)
import Hex.Parse.TokenParser.Class (ExpandedToken, MonadTokenParse(..))
import Hex.Lex (LexError)
import qualified Hex.Lex as Lex
import Path (File, Rel, Path)
import Hex.Resolve (PrimitiveToken, ResolvedToken)

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
  = App {unApp :: TeXParseT ExpandingStream (ExceptT AppError (StateT Conf.Config IO)) a}
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

instance MonadTokenParse App where

  parseError :: P.ParseError -> App a
  parseError = App . parseError

  satisfyThen :: (PrimitiveToken -> Maybe a) -> App a
  satisfyThen = App . satisfyThen

  withInhibition :: App a -> App a
  withInhibition = App . withInhibition . unApp

  takeWhileP :: (PrimitiveToken -> Bool) -> App (Seq PrimitiveToken)
  takeWhileP = App . takeWhileP

  takeLexToken :: App Lex.Token
  takeLexToken = App takeLexToken

  takeAndResolveLexToken :: App (Lex.Token, ResolvedToken)
  takeAndResolveLexToken = App takeAndResolveLexToken

  takeAndExpandResolvedToken :: App (Lex.Token, ExpandedToken)
  takeAndExpandResolvedToken = App takeAndExpandResolvedToken

  pushSkipState :: ConditionBodyState -> App ()
  pushSkipState = App . pushSkipState

  peekSkipState :: App (Maybe ConditionBodyState)
  peekSkipState = App peekSkipState

  popSkipState :: App (Maybe ConditionBodyState)
  popSkipState = App popSkipState

  inputPath :: Path Rel File -> App ()
  inputPath = App . inputPath

  insertLexTokens :: Seq Lex.Token -> App ()
  insertLexTokens = App . insertLexTokens

instance MonadSlog App where
  sLog msg = do
    loggerSet <- gets Conf.internalLoggerSet
    liftIO $ Log.pushLogStrLn loggerSet (Log.toLogStr msg)

  sTime = liftIO getCurrentTime

runApp
  :: MonadIO m
  => App a
  -> Conf.Config
  -> ExpandingStream
  -> m (Either AppError (ExpandingStream, a))
runApp a c s =
  let texParseT = unApp a
      exceptT = runTeXParseTEmbedded texParseT s
      stateT = runExceptT exceptT
      io = liftIO $ evalStateT stateT c
  in io


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
