{-# LANGUAGE RankNTypes #-}
module HeX.Command.Run where

import Control.Monad.Except (runExceptT)
import DVI.Document (Instruction, parseInstructions)
import DVI.Encode (encode)
import DVI.Instruction (DVIError, EncodableInstruction)
import Data.Byte (ByteError)
import qualified Data.ByteString.Lazy as BS.L
import Data.Path (PathError)
import qualified Data.Text as Tx
import HeX.Box
import HeX.BreakList
import HeX.Categorise
import HeX.Command.Build
import HeX.Command.Common
import qualified HeX.Config as Conf
import qualified HeX.Config.Codes as Code
import HeX.Evaluate (EvaluationError)
import qualified HeX.Lex
import qualified HeX.Parse as HP
import qualified HeX.Quantity as Quantity
import HeXlude
import TFM (TFMError)
import qualified Text.Megaparsec as P
import Control.Monad.Trans.Writer.CPS as Wr

type AppError
  = Variant
      '[ BuildError
       , Conf.ConfigError
       , EvaluationError
       , PathError
       , HP.ExpansionError
       , TFMError
       ]

newtype App a
  = App {unApp :: ExceptT AppError (StateT HP.ExpandedStream IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState HP.ExpandedStream
    , MonadIO
    , MonadError AppError
    )

usableCatLookup :: Code.CharCode -> Code.CatCode
usableCatLookup = Code.catLookup Code.usableCatCodes

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
  deriving (Show, Eq)

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

renderWithMode :: Mode -> BS.L.ByteString -> IO Text
renderWithMode mode input = case mode of
    CatMode -> pure $ show $ codesToCharCats usableCatLookup input -- TODO: Render nicely.
    LexMode -> pure $ show $ HeX.Lex.codesToLexTokens usableCatLookup input
    ResolveMode -> pure $ show $ HP.codesToResolvedTokens usableCatLookup HP.defaultCSMap input
    ExpandMode -> makeStream >>= streamAsExpandedTokens <&> renderLoopParserResult
    CommandMode -> makeStream >>= streamAsCommands <&> renderLoopParserResult
    ParaListMode -> makeStream >>= renderStreamUnsetPara
    ParaSetMode -> makeStream >>= renderStreamSetPara
    PageListMode -> makeStream >>= renderStreamPageList
    PageMode -> makeStream >>= renderStreamPages
    SemanticDVIMode -> makeStream >>= renderStreamSemanticDVI
    RawDVIMode -> makeStream >>= renderStreamRawDVI
    DVIBytesMode -> panic "You probably don't want to render the DVI bytes as text"
  where
    makeStream = HP.newExpandStream Nothing input

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
   . Monad m
  => HP.SimpleParsecT HP.ExpandedStream (ExceptT (Variant HP.TeXStreamE) m) a
  -> HP.ExpandedStream
  -> m (Text, [a])
loopParser parser s = Wr.runWriterT (go s)
  where
    go :: HP.ExpandedStream -> Wr.WriterT [a] m Text
    go stream = do
      errOrA <- lift (runExceptT (HP.runSimpleRunParserT' parser stream))
      case errOrA of
        Left err ->
          pure $ show err
        Right (s1, c) -> do
          Wr.tell [c]
          go s1

renderLoopParserResult :: Show a => (Text, [a]) -> Text
renderLoopParserResult (errMsg, xs) =
    "Ended with message:\n\t \"" <> errMsg <> "\n\n" <> "Got results: " <> Tx.concat (intersperse "\n" (show <$> xs))

streamAsExpandedTokens :: HP.ExpandedStream -> IO (Text, [HP.PrimitiveToken])
streamAsExpandedTokens = loopParser P.anySingle

-- Command.
streamAsCommands :: HP.ExpandedStream -> IO (Text, [HP.Command])
streamAsCommands = loopParser HP.parseCommand

runApp
  :: HP.ExpandedStream
  -> App a
  -> IO a
runApp s f = evalStateT (runExceptT $ unApp f) s >>= strEitherToIO
  where
    strEitherToIO = \case
      Left err -> panic $ show err
      Right v -> pure v

-- Paragraph list.
extractUnsetParaApp :: App HList
extractUnsetParaApp =
  (\(ParaResult _ hList) -> hList) <$> extractPara HP.Indent

renderStreamUnsetPara :: HP.ExpandedStream -> IO Text
renderStreamUnsetPara s = do
  HList elemSeq <- runApp s extractUnsetParaApp
  pure $ describeLined elemSeq

-- Paragraph boxes.
streamToParaBoxes :: HP.ExpandedStream -> IO (Seq (Box HBox))
streamToParaBoxes s =
  runApp s $ extractUnsetParaApp <&> hListToParaLineBoxes >>= readOnConfState

renderStreamSetPara :: HP.ExpandedStream -> IO Text
renderStreamSetPara s = streamToParaBoxes s <&> describeDoubleLined

-- Pages list.
renderStreamPageList :: HP.ExpandedStream -> IO Text
renderStreamPageList s = do
  MainVModeResult (VList vList) <- runApp s extractMainVList
  pure $ describeLined vList

-- Pages boxes.
streamToPages :: HP.ExpandedStream -> IO (Seq Page, Conf.IntParamVal 'HP.Mag)
streamToPages s = runApp s extractBreakAndSetVList

renderStreamPages :: HP.ExpandedStream -> IO Text
renderStreamPages s = streamToPages s <&> fst <&> describeLined

-- DVI instructions.
streamToSemanticDVI :: HP.ExpandedStream -> IO (Seq Instruction, Conf.IntParamVal 'HP.Mag)
streamToSemanticDVI s = do
  (pages, mag) <- streamToPages s
  pure (pagesToDVI pages, mag)

renderStreamSemanticDVI :: HP.ExpandedStream -> IO Text
renderStreamSemanticDVI s = streamToSemanticDVI s <&> fst <&> describeLined

-- Raw DVI instructions.
streamToRawDVI :: HP.ExpandedStream -> IO (Seq EncodableInstruction)
streamToRawDVI s = do
  (instrs, mag) <- streamToSemanticDVI s
  strEitherToIO $ parseInstructions instrs $ Quantity.unInt $ Conf.unIntParam mag
  where
    strEitherToIO :: Either (Variant '[ByteError, DVIError, PathError]) v -> IO v
    strEitherToIO = \case
      Left err -> panic $ show err
      Right v -> pure v

renderStreamRawDVI :: HP.ExpandedStream -> IO Text
renderStreamRawDVI s = streamToRawDVI s <&> describeLined

-- DVI byte strings.
streamToDVIBytes :: HP.ExpandedStream -> IO ByteString
streamToDVIBytes s = streamToRawDVI s <&> encode
