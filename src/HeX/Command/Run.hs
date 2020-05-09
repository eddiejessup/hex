{-# LANGUAGE RankNTypes #-}
module Hex.Command.Run where

import Control.Monad.Trans.Writer.CPS as Wr
import DVI.Document (Instruction, parseInstructions)
import DVI.Encode (encode)
import DVI.Instruction (DVIError, EncodableInstruction)
import Data.Byte (ByteError)
import Data.Path (PathError)
import Hex.Box
import Hex.Command.Build
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate (EvaluationError)
import qualified Hex.Parse as HP
import qualified Hex.Quantity as Quantity
import Hexlude
import TFM (TFMError)

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

loopParser
  :: forall m e a
   . ( Monad m
     , AsType HP.ParseError e
     )
  => HP.TeXParseT HP.ExpandingStream (ExceptT e m) a
  -> HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe e, [a])
loopParser parser s = do
  ((postLoopStream, mayErr), xs) <- Wr.runWriterT (go s)
  pure (postLoopStream, mayErr, xs)
  where
    go :: HP.ExpandingStream -> Wr.WriterT [a] m (HP.ExpandingStream, Maybe e)
    go stream = do
      errOrA <- lift $ runExceptT (HP.runTeXParseTEmbedded parser stream)
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
  :: ( MonadIO m

     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType Data.Path.PathError e
     )
  => HP.ExpandingStream
  -> Conf.Config
  -> m (HP.ExpandingStream, Maybe e, [HP.PrimitiveToken])
expandingStreamAsPrimTokens s =
  evalStateT (loopParser HP.anySingle s)

-- Command.
expandingStreamAsCommands
  :: ( MonadIO m

     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType Data.Path.PathError e
     )
  => HP.ExpandingStream
  -> Conf.Config
  -> m (HP.ExpandingStream, Maybe e, [HP.Command])
expandingStreamAsCommands s =
  evalStateT (loopParser HP.parseCommand s)

-- Paragraph list.
renderStreamUnsetPara
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m Text
renderStreamUnsetPara s = do
  (_, hList, _) <- extractPara HP.Indent s
  pure $ renderDescribed hList

-- Paragraph boxes.
streamToParaBoxes
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m (Seq (Box HBox))
streamToParaBoxes s = do
  (_, hList, _) <- extractPara HP.Indent s
  hListToParaLineBoxes hList

renderStreamSetPara
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m Text
renderStreamSetPara s = do
  boxes <- streamToParaBoxes s
  pure $ renderLines $ describeRelFoldable 0 boxes

-- Pages list.
renderStreamPageList
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m Text
renderStreamPageList s = do
  (_, vList) <- extractMainVList s
  pure $ renderDescribed vList

-- Pages boxes.
renderStreamPages
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m Text
renderStreamPages s = do
  (_, pages, _mag) <- extractBreakAndSetMainVList s
  pure $ renderLines $ describeRelFoldable 0 pages

-- DVI instructions.
streamToSemanticDVI
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m (Seq Instruction, Conf.IntParamVal 'HP.Mag)
streamToSemanticDVI s = do
  (_, pages, _mag) <- extractBreakAndSetMainVList s
  sLog "In streamToSemanticDVI, done extractBreakAndSetMainVList"
  pure (pagesToDVI pages, _mag)

renderStreamSemanticDVI
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m Text
renderStreamSemanticDVI s = do
  (semDVI, _mag) <- streamToSemanticDVI s
  pure $ renderLines $ describeRelFoldable 0 semDVI

-- Raw DVI instructions.
streamToRawDVI
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e
     , AsType ByteError e
     , AsType DVIError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m (Seq EncodableInstruction)
streamToRawDVI s = do
  (semDVI, _mag) <- streamToSemanticDVI s
  sLog "In streamToRawDVI, done streamToSemanticDVI"
  parseInstructions semDVI (Quantity.unInt $ Conf.unIntParam _mag)

renderStreamRawDVI
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e
     , AsType ByteError e
     , AsType DVIError e

     , MonadIO m
     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m Text
renderStreamRawDVI s = do
  rawDVI <- streamToRawDVI s
  pure $ renderLines $ describeRelFoldable 0 rawDVI

-- DVI byte strings.
streamToDVIBytes
  :: ( MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e
     , AsType ByteError e
     , AsType DVIError e

     , MonadIO m

     , MonadSlog m
     )
  => HP.ExpandingStream
  -> m ByteString
streamToDVIBytes s = do
  rawDVI <- streamToRawDVI s
  sLog "In streamToDVIBytes, done streamToRawDVI"
  pure $ encode rawDVI
