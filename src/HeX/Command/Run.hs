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
import Hex.Command.Common
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate (EvaluationError)
import qualified Hex.Lex
import qualified Hex.Parse as HP
import qualified Hex.Quantity as Quantity
import Hexlude
import TFM (TFMError)
import qualified Text.Megaparsec as P
import Control.Monad.Trans.Writer.CPS as Wr

type AppErrorE =
  '[ BuildError
   , Conf.ConfigError
   , EvaluationError
   , PathError
   , HP.ExpansionError
   , HP.ResolutionError
   , TFMError
   , HP.ParseError
   , ByteError
   , DVIError
   ]

type AppError
  = Variant AppErrorE

data FlatAppError
  = FlatBuildError BuildError
  | FlatConfigError Conf.ConfigError
  | FlatEvaluationError EvaluationError
  | FlatPathError PathError
  | FlatExpansionError HP.ExpansionError
  | FlatResolutionError HP.ResolutionError
  | FlatTFMError TFMError
  | FlatParseError HP.ParseError
  | FlatByteError ByteError
  | FlatDVIError DVIError
  deriving stock Show

flattenAppError :: AppError -> FlatAppError
flattenAppError appError =
  case errOrVoid of
    Left err -> err
  where
    errOrVoid :: Either FlatAppError Void
    errOrVoid = do
      emptyV <- catchey FlatBuildError appError
        >>= catchey FlatConfigError
        >>= catchey FlatEvaluationError
        >>= catchey FlatPathError
        >>= catchey FlatExpansionError
        >>= catchey FlatResolutionError
        >>= catchey FlatTFMError
        >>= catchey FlatParseError
        >>= catchey FlatByteError
        >>= catchey FlatDVIError
      pure $ preposterous emptyV

    catchey f v = case catch v of
      Left other -> pure other
      Right e -> throwError $ f e

newtype App a
  = App {unApp :: ExceptT AppError (StateT HP.ExpandingStream IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState HP.ExpandingStream
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

-- renderWithMode :: Mode -> BS.L.ByteString -> IO (Either AppError Text)
-- renderWithMode mode input = case mode of
--     CatMode -> pure $ Right $ show $ usableCodesToCharCats input -- TODO: Render nicely.
--     LexMode -> pure $ Right $ show $ usableCodesToLexTokens input
--     ResolveMode -> pure $ Right $ show $ HP.usableCodesToResolvedTokens input
--     ExpandMode -> makeStream >>= expandingStreamAsPrimTokens <&> renderLoopParserResult
--     CommandMode -> makeStream >>= expandingStreamAsCommands <&> renderLoopParserResult
--     ParaListMode -> makeStream >>= renderStreamUnsetPara
--     ParaSetMode -> makeStream >>= renderStreamSetPara
--     PageListMode -> makeStream >>= renderStreamPageList
--     PageMode -> makeStream >>= renderStreamPages
--     SemanticDVIMode -> makeStream >>= renderStreamSemanticDVI
--     RawDVIMode -> makeStream >>= renderStreamRawDVI
--     DVIBytesMode -> panic "You probably don't want to render the DVI bytes as text"
--   where
--     makeStream = HP.newExpandStream Nothing input

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
  :: forall m e a
   . ( Monad m
     , e `CouldBe` HP.ParseError
     )
  => HP.SimpleParsecT HP.ExpandingStream (ExceptT (Variant e) m) a
  -> HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe (Variant e), [a])
loopParser parser s = do
  ((postLoopStream, mayErr), xs) <- Wr.runWriterT (go s)
  pure (postLoopStream, mayErr, xs)
  where
    go :: HP.ExpandingStream -> Wr.WriterT [a] m (HP.ExpandingStream, Maybe (Variant e))
    go stream = do
      errOrA <- lift (runExceptT (HP.runSimpleRunParserT' parser stream))
      case errOrA of
        Left err ->
          pure (stream, Just err)
        Right (postParseStream, a) -> do
          Wr.tell [a]
          -- Try to fetch a lex token. If we can, we have more input, so try to
          -- parse some more. This won't always work, but it's a decent heuristic.
          case HP.fetchLexToken postParseStream of
            Nothing -> pure (stream, Nothing)
            Just _ -> go postParseStream

flattenedLoopParser
  :: Monad m
  => HP.SimpleParsecT HP.ExpandingStream (ExceptT AppError m) a
  -> HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe FlatAppError, [a])
flattenedLoopParser parser s = do
  (postS, mayAppError, xs) <- loopParser parser s
  pure (postS, flattenAppError <$> mayAppError, xs)

renderLoopParserResult :: Show a => (FlatAppError, [a]) -> Either FlatAppError Text
renderLoopParserResult (errMsg, xs) =
    Left errMsg
    -- "Ended with message:\n\t \"" <> errMsg <> "\n\n" <> "Got results: " <> Tx.concat (intersperse "\n" (show <$> xs))

expandingStreamAsPrimTokens
  :: MonadIO m
  => HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe FlatAppError, [HP.PrimitiveToken])
expandingStreamAsPrimTokens = flattenedLoopParser P.anySingle

-- Command.
expandingStreamAsCommands
  :: MonadIO m
  => HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe FlatAppError, [HP.Command])
expandingStreamAsCommands = flattenedLoopParser HP.parseCommand

runApp
  :: MonadIO m
  => HP.ExpandingStream
  -> App a
  -> m (Either FlatAppError a)
runApp s f = do
  liftIO (evalStateT (runExceptT $ unApp f) s)
    <&> first flattenAppError

-- Paragraph list.
extractUnsetParaApp :: App HList
extractUnsetParaApp =
  (\(ParaResult _ hList) -> hList) <$> extractPara HP.Indent

renderStreamUnsetPara
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError Text)
renderStreamUnsetPara s =
  runApp s extractUnsetParaApp <&> \case
    Left err ->
      Left err
    Right (HList elemSeq) ->
      Right $ describeLined elemSeq

-- Paragraph boxes.
streamToParaBoxes
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError (Seq (Box HBox)))
streamToParaBoxes s =
  do
  errOrBoxes <- runApp s $ do
    hList <- extractUnsetParaApp
    readOnConfState (hListToParaLineBoxes hList)
  pure $ case errOrBoxes of
    Left err ->
      Left err
    Right boxes ->
      Right boxes

renderStreamSetPara
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError Text)
renderStreamSetPara s =
  streamToParaBoxes s <&> \case
    Left err ->
      Left err
    Right boxes ->
      Right $ describeDoubleLined boxes

-- Pages list.
renderStreamPageList
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError Text)
renderStreamPageList s =
  runApp s extractMainVList <&> \case
    Left err ->
      Left err
    Right (MainVModeResult (VList vList)) ->
      Right $ describeLined vList

-- Pages boxes.
streamToPages
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError (Seq Page, Conf.IntParamVal 'HP.Mag))
streamToPages s = runApp s extractBreakAndSetVList

renderStreamPages
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError Text)
renderStreamPages s =
  streamToPages s <&> \case
    Left err ->
      Left err
    Right (pages, _mag) ->
      Right $ describeLined pages

-- DVI instructions.
streamToSemanticDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError (Seq Instruction, Conf.IntParamVal 'HP.Mag))
streamToSemanticDVI s =
  streamToPages s <&> \case
    Left err ->
      Left err
    Right (pages, mag) ->
      Right (pagesToDVI pages, mag)

renderStreamSemanticDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError Text)
renderStreamSemanticDVI s =
  streamToSemanticDVI s <&> \case
    Left err ->
      Left err
    Right (semDVI, _mag) ->
      Right $ describeLined semDVI

-- Raw DVI instructions.
streamToRawDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError (Seq EncodableInstruction))
streamToRawDVI s =
  streamToSemanticDVI s >>= \case
    Left err ->
      pure $ Left err
    Right (semDVI, mag) ->
      let magInt = Quantity.unInt $ Conf.unIntParam mag
      in case runExcept @AppError (parseInstructions semDVI magInt) of
        Left err ->
          pure $ Left $ flattenAppError err
        Right rawDVI ->
          pure $ Right rawDVI

renderStreamRawDVI
  :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError Text)
renderStreamRawDVI s =
  streamToRawDVI s <&> \case
    Left err -> Left err
    Right rawDVI -> Right $ describeLined rawDVI

-- DVI byte strings.
streamToDVIBytes :: MonadIO m
  => HP.ExpandingStream
  -> m (Either FlatAppError ByteString)
streamToDVIBytes s =
  streamToRawDVI s <&> \case
    Left err -> Left err
    Right rawDVI -> Right $ encode rawDVI
