module Hex.Run where

import Control.Monad.Trans.Writer.CPS as Wr
import DVI.Document (Instruction, parseInstructions)
import DVI.Encode (dviEncode)
import DVI.Instruction (DVIError, EncodableInstruction)
import Data.Byte (ByteError)
import Data.Path (PathError)
import Hex.Box
import Hex.Build.Class
import Hex.Build.Command
import Hex.Build.ListBuilderT
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate (EvaluationError)
import qualified Hex.Quantity as Quantity
import Hexlude
import TFM (TFMError)
import qualified Hex.Parse.TokenParser.Class as P
import qualified Hex.Parse.TokenParser.Combinators as P
import qualified Hex.Parse.CommandParser.Command as P
import qualified Hex.Parse.TokenParser.ParseT as ParseT
import qualified Hex.Parse.Stream.Expanding as S
import qualified Hex.Parse.Stream.Expanding.Parse ()
import qualified Hex.Parse.Stream.Class as S
import qualified Hex.Resolve.Resolve as R
import qualified Hex.Parse.AST as AST
import qualified Hex.Resolve.Token as Tok
import qualified Hex.Lex as Lex

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

loopParser
  :: forall m e a
   . ( Monad m
     , AsType P.ParseError e
     )
  => S.ExpandingStream
  -> ParseT.TeXParseT S.ExpandingStream (ExceptT e m) a
  -> m (S.ExpandingStream, Maybe e, [a])
loopParser s parser = do
  ((postLoopStream, mayErr), xs) <- Wr.runWriterT (go s)
  pure (postLoopStream, mayErr, xs)
  where
    go :: S.ExpandingStream -> Wr.WriterT [a] m (S.ExpandingStream, Maybe e)
    go stream =
      lift (runExceptT (ParseT.runTeXParseTEmbedded parser stream)) >>= \case
        Left err ->
          pure (stream, Just err)
        Right (postParseStream, a) -> do
          Wr.tell [a]
          go postParseStream
          -- -- Try to fetch a lex token. If we can, we have more input, so try to
          -- -- parse some more. This won't always work, but it's a decent heuristic.
          -- case S.extractLexToken postParseStream Code.usableCatLookup of
          --   Nothing -> pure (stream, Nothing)
          --   Just _ ->

expandingStreamAsPrimTokens
  :: ( MonadIO m

     , MonadState st m
     , HasType Conf.Config st

     , AsType EvaluationError e
     , AsType Conf.ConfigError e
     , AsType R.ResolutionError e
     , AsType P.ParseError e
     , AsType S.ExpansionError e
     , AsType Data.Path.PathError e
     , AsType Lex.LexError e
     )
  => S.ExpandingStream
  -> m (S.ExpandingStream, Maybe e, [Tok.PrimitiveToken])
expandingStreamAsPrimTokens s = loopParser s P.anySingle

-- -- Command.
-- expandingStreamAsCommands
--   :: ( MonadIO m
--      , MonadSlog m

--      , MonadState st m
--      , HasType Conf.Config st

--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType R.ResolutionError e
--      , AsType P.ParseError e
--      , AsType S.ExpansionError e
--      , AsType Data.Path.PathError e
--      )
--   => m (Maybe e, [AST.Command])
-- expandingStreamAsCommands = loopParser P.parseCommand

-- -- Paragraph list.
-- renderStreamParaList
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Text)
-- renderStreamParaList = do
--   (hList, _) <- extractParaListImpl Tok.Indent
--   pure (endS, renderDescribed hList)

-- -- Paragraph boxes.
-- streamToParaBoxes
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Seq (Box HBox))
-- streamToParaBoxes = do
--   (endS, hList, _) <- extractParaListImpl Tok.Indent
--   boxes <- hListToParaLineBoxes hList
--   pure (endS, boxes)

-- renderStreamSetPara
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m


--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Text)
-- renderStreamSetPara = do
--   (endS, boxes) <- streamToParaBoxes
--   pure (endS, renderLines $ describeRelFoldable 0 boxes)

-- -- Pages list.
-- renderStreamPageList
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Text)
-- renderStreamPageList = do
--   (endS, vList) <- extractMainVList
--   pure (endS, renderDescribed vList)

-- -- Pages boxes.
-- renderStreamPages
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Text)
-- renderStreamPages = do
--   (endS, pages, _mag) <- extractBreakAndSetMainVList
--   pure (endS, renderLines $ describeRelFoldable 0 pages)

-- -- DVI instructions.
-- streamToSemanticDVI
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Seq Instruction, Conf.IntParamVal 'Tok.Mag)
-- streamToSemanticDVI = do
--   (endS, pages, _mag) <- extractBreakAndSetMainVList
--   sLog "In streamToSemanticDVI, done extractBreakAndSetMainVList"
--   pure (endS, pagesToDVI pages, _mag)

-- renderStreamSemanticDVI
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Text)
-- renderStreamSemanticDVI = do
--   (endS, semDVI, _mag) <- streamToSemanticDVI
--   pure (endS, renderLines $ describeRelFoldable 0 semDVI)

-- -- Raw DVI instructions.
-- streamToRawDVI
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e
--      , AsType ByteError e
--      , AsType DVIError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Seq EncodableInstruction)
-- streamToRawDVI = do
--   (endS, semDVI, _mag) <- streamToSemanticDVI
--   sLog "In streamToRawDVI, done streamToSemanticDVI"
--   rawDVI <- parseInstructions semDVI (Quantity.unInt $ Conf.unIntParam _mag)
--   pure (endS, rawDVI)

-- renderStreamRawDVI
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e
--      , AsType ByteError e
--      , AsType DVIError e

--      , P.MonadTokenParse m

--      , MonadIO m
--      , MonadSlog m
--      )
--   => m (Text)
-- renderStreamRawDVI = do
--   (endS, rawDVI) <- streamToRawDVI
--   pure (endS, renderLines $ describeRelFoldable 0 rawDVI)

-- -- DVI byte strings.
-- streamToDVIBytes
--   :: ( MonadState st m
--      , HasType Conf.Config st

--      , MonadError e m
--      , AsType EvaluationError e
--      , AsType Conf.ConfigError e
--      , AsType P.ParseError e
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e
--      , AsType ByteError e
--      , AsType DVIError e

--      , P.MonadTokenParse m

--      , MonadIO m

--      , MonadSlog m
--      )
--   => m (ByteString)
-- streamToDVIBytes = do
--   (endS, rawDVI) <- streamToRawDVI
--   sLog "In streamToDVIBytes, done streamToRawDVI"
--   pure (endS, dviEncode rawDVI)
