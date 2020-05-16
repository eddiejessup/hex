{-# LANGUAGE RankNTypes #-}
module Hex.Command.Run where

import Control.Monad.Trans.Writer.CPS as Wr
import DVI.Document (Instruction, parseInstructions)
import DVI.Encode (dviEncode)
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
    go stream =
      lift (runExceptT (HP.runTeXParseTEmbedded parser stream)) >>= \case
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
     , MonadSlog m

     , MonadState st m
     , HasType Conf.Config st

     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType Data.Path.PathError e
     )
  => HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe e, [HP.PrimitiveToken])
expandingStreamAsPrimTokens = loopParser HP.anySingle

-- Command.
expandingStreamAsCommands
  :: ( MonadIO m
     , MonadSlog m

     , MonadState st m
     , HasType Conf.Config st

     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType HP.ResolutionError e
     , AsType HP.ParseError e
     , AsType HP.ExpansionError e
     , AsType Data.Path.PathError e
     )
  => HP.ExpandingStream
  -> m (HP.ExpandingStream, Maybe e, [HP.Command])
expandingStreamAsCommands = loopParser HP.parseCommand

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
  -> m (HP.ExpandingStream, Text)
renderStreamUnsetPara s = do
  (endS, hList, _) <- extractPara HP.Indent s
  pure (endS, renderDescribed hList)

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
  -> m (HP.ExpandingStream, Seq (Box HBox))
streamToParaBoxes s = do
  (endS, hList, _) <- extractPara HP.Indent s
  boxes <- hListToParaLineBoxes hList
  pure (endS, boxes)

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
  -> m (HP.ExpandingStream, Text)
renderStreamSetPara s = do
  (endS, boxes) <- streamToParaBoxes s
  pure (endS, renderLines $ describeRelFoldable 0 boxes)

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
  -> m (HP.ExpandingStream, Text)
renderStreamPageList s = do
  (endS, vList) <- extractMainVList s
  pure (endS, renderDescribed vList)

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
  -> m (HP.ExpandingStream, Text)
renderStreamPages s = do
  (endS, pages, _mag) <- extractBreakAndSetMainVList s
  pure (endS, renderLines $ describeRelFoldable 0 pages)

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
  -> m (HP.ExpandingStream, Seq Instruction, Conf.IntParamVal 'HP.Mag)
streamToSemanticDVI s = do
  (endS, pages, _mag) <- extractBreakAndSetMainVList s
  sLog "In streamToSemanticDVI, done extractBreakAndSetMainVList"
  pure (endS, pagesToDVI pages, _mag)

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
  -> m (HP.ExpandingStream, Text)
renderStreamSemanticDVI s = do
  (endS, semDVI, _mag) <- streamToSemanticDVI s
  pure (endS, renderLines $ describeRelFoldable 0 semDVI)

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
  -> m (HP.ExpandingStream, Seq EncodableInstruction)
streamToRawDVI s = do
  (endS, semDVI, _mag) <- streamToSemanticDVI s
  sLog "In streamToRawDVI, done streamToSemanticDVI"
  rawDVI <- parseInstructions semDVI (Quantity.unInt $ Conf.unIntParam _mag)
  pure (endS, rawDVI)

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
  -> m (HP.ExpandingStream, Text)
renderStreamRawDVI s = do
  (endS, rawDVI) <- streamToRawDVI s
  pure (endS, renderLines $ describeRelFoldable 0 rawDVI)

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
  -> m (HP.ExpandingStream, ByteString)
streamToDVIBytes s = do
  (endS, rawDVI) <- streamToRawDVI s
  sLog "In streamToDVIBytes, done streamToRawDVI"
  pure (endS, dviEncode rawDVI)
