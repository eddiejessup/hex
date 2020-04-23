{-# LANGUAGE RankNTypes #-}
module Hex.Command.Run where

import DVI.Document (Instruction, parseInstructions)
import DVI.Encode (encode)
import DVI.Instruction (DVIError, EncodableInstruction)
import Data.Byte (ByteError)
import qualified Data.ByteString.Lazy as BS.L
import Data.Path (PathError)
import Hex.Box
import Hex.BreakList
import Hex.Categorise
import Hex.Command.Build
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate (EvaluationError)
import qualified Hex.Parse as HP
import qualified Hex.Quantity as Quantity
import Hexlude
import TFM (TFMError)
import qualified Text.Megaparsec as P
import Control.Monad.Trans.Writer.CPS as Wr

data AppError
  = BuildError BuildError
  | ConfigError Conf.ConfigError
  | EvaluationError EvaluationError
  | PathError PathError
  | ExpansionError HP.ExpansionError
  | ResolutionError HP.ResolutionError
  | TFMError TFMError
  | ParseError HP.ParseError
  | ByteError ByteError
  | DVIError DVIError
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

data Mode
  = CatMode
  | LexMode
  | ResolveMode
  | ExpandMode
  | CommandMode
  | ParaListMode
  | ParaSetMode
  | PageListMode
  | PageMode
  | SemanticDVIMode
  | RawDVIMode
  | DVIBytesMode
  deriving stock (Show, Eq)

readMode :: (IsString s, Eq s) => s -> Maybe Mode
readMode = \case
    "cat" -> Just CatMode
    "lex" -> Just LexMode
    "resolve" -> Just ResolveMode
    "expand" -> Just ExpandMode
    "command" -> Just CommandMode
    "paralist" -> Just ParaListMode
    "paraset" -> Just ParaSetMode
    "pagelist" -> Just PageListMode
    "page" -> Just PageMode
    "dvi" -> Just SemanticDVIMode
    "rawdvi" -> Just RawDVIMode
    "bytes" -> Just DVIBytesMode
    _ -> Nothing

-- Cat
benchCatBS
  :: ( MonadIO m
     )
  => BS.L.ByteString
  -> m ()
benchCatBS xs = case extractCharCat (Code.catLookup Code.usableCatCodes) xs of
  Just (_, xs') ->
    benchCatBS xs'
  Nothing ->
    pure ()

-- Expand.
loopParser
  :: forall m a
   . ( Monad m
     )
  => HP.SimpleParsecT HP.ExpandingStream (ExceptT AppError m) a
  -> HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe AppError, [a])
loopParser parser s = do
  ((postLoopStream, mayErr), xs) <- Wr.runWriterT (go s)
  pure (postLoopStream, mayErr, xs)
  where
    go :: HP.ExpandingStream -> Wr.WriterT [a] m (HP.ExpandingStream, Maybe AppError)
    go stream = do
      errOrA <- lift $ runExceptT (HP.runSimpleRunParserT' parser stream)
      case errOrA of
        Left err ->
          pure (stream, Just err)
        Right (postParseStream, a) -> do
          Wr.tell [a]
          -- Try to fetch a lex token. If we can, we have more input, so try to
          -- parse some more. This won't always work, but it's a decent heuristic.
          case HP.extractLexToken postParseStream Code.usableCatLookup of
            Nothing -> pure (stream, Nothing)
            Just _ -> go postParseStream

expandingStreamAsPrimTokens
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (HP.ExpandingStream, Maybe AppError, [HP.PrimitiveToken])
expandingStreamAsPrimTokens s =
  evalStateT (loopParser P.anySingle s)

-- Command.
expandingStreamAsCommands
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (HP.ExpandingStream, Maybe AppError, [HP.Command])
expandingStreamAsCommands s =
  evalStateT (loopParser HP.parseCommand s)

runApp
  :: MonadIO m
  => Conf.Config
  -> App a
  -> m (Either AppError a)
runApp c f =
  liftIO (evalStateT (runExceptT $ unApp f) c)

-- Paragraph list.
extractUnsetParaApp :: HP.ExpandingStream -> App HList
extractUnsetParaApp s =
  (\(_, ParaResult _ hList) -> hList) <$> extractPara HP.Indent s

renderStreamUnsetPara
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError Text)
renderStreamUnsetPara s c =
  runApp c (extractUnsetParaApp s) <&> \case
    Left err ->
      Left err
    Right (HList elemSeq) ->
      Right $ describeLined elemSeq

-- Paragraph boxes.
streamToParaBoxes
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError (Seq (Box HBox)))
streamToParaBoxes s c =
  runApp c $ extractUnsetParaApp s >>= hListToParaLineBoxes

renderStreamSetPara
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError Text)
renderStreamSetPara s c =
  streamToParaBoxes s c <&> \case
    Left err ->
      Left err
    Right boxes ->
      Right $ describeDoubleLined boxes

-- Pages list.
renderStreamPageList
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError Text)
renderStreamPageList s c =
  runApp c (extractMainVList s) <&> \case
    Left err ->
      Left err
    Right (_, MainVModeResult (VList vList)) ->
      Right $ describeLined vList

-- Pages boxes.
streamToPages
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError (Seq Page, Conf.IntParamVal 'HP.Mag))
streamToPages s c =
  runApp c $ extractBreakAndSetMainVList s
    <&> \(_, pgs, mag_) -> (pgs, mag_)

renderStreamPages
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError Text)
renderStreamPages s c =
  streamToPages s c <&> \case
    Left err ->
      Left err
    Right (pages, _mag) ->
      Right $ describeLined pages

-- DVI instructions.
streamToSemanticDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError (Seq Instruction, Conf.IntParamVal 'HP.Mag))
streamToSemanticDVI s c =
  streamToPages s c <&> \case
    Left err ->
      Left err
    Right (pages, mag) ->
      Right (pagesToDVI pages, mag)

renderStreamSemanticDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError Text)
renderStreamSemanticDVI s c =
  streamToSemanticDVI s c <&> \case
    Left err ->
      Left err
    Right (semDVI, _mag) ->
      Right $ describeLined semDVI

-- Raw DVI instructions.
streamToRawDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError (Seq EncodableInstruction))
streamToRawDVI s c =
  streamToSemanticDVI s c >>= \case
    Left err ->
      pure $ Left err
    Right (semDVI, mag) ->
      let magInt = Quantity.unInt $ Conf.unIntParam mag
      in case runExcept @AppError (parseInstructions semDVI magInt) of
        Left err ->
          pure $ Left err
        Right rawDVI ->
          pure $ Right rawDVI

renderStreamRawDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError Text)
renderStreamRawDVI s c =
  streamToRawDVI s c <&> \case
    Left err -> Left err
    Right rawDVI -> Right $ describeLined rawDVI

-- DVI byte strings.
streamToDVIBytes :: MonadIO m
  => HP.ExpandingStream
  -> Conf.Config
  -> m (Either AppError ByteString)
streamToDVIBytes s c =
  streamToRawDVI s c <&> \case
    Left err -> Left err
    Right rawDVI -> Right $ encode rawDVI
