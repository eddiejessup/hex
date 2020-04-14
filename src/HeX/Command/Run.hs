{-# LANGUAGE RankNTypes #-}
module Hex.Command.Run where

import DVI.Document (Instruction, parseInstructions)
import DVI.Encode (encode)
import DVI.Instruction (DVIError, EncodableInstruction)
import Data.Byte (ByteError)
import qualified Data.ByteString.Lazy as BS.L
import Data.Path (PathError)
import qualified Data.Text as Tx
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
   ]

type AppError
  = Variant AppErrorE

newtype App a
  = App {unApp :: ExceptT AppError (StateT HP.ExpandingStream IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState HP.ExpandingStream
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
    LexMode -> pure $ show $ Hex.Lex.codesToLexTokens usableCatLookup input
    ResolveMode -> pure $ show $ HP.codesToResolvedTokens usableCatLookup HP.defaultCSMap input
    ExpandMode -> makeStream >>= expandingStreamAsPrimTokens <&> renderLoopParserResult
    CommandMode -> makeStream >>= expandingStreamAsCommands <&> renderLoopParserResult
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
  :: forall m e a
   . ( Monad m
     , e `CouldBe` HP.ParseError
     , Show (Variant e)
     )
  => HP.SimpleParsecT HP.ExpandingStream (ExceptT (Variant e) m) a
  -> HP.ExpandingStream
  -> m (Text, [a])
loopParser parser s = Wr.runWriterT (go s)
  where
    go :: HP.ExpandingStream -> Wr.WriterT [a] m Text
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

expandingStreamAsPrimTokens :: HP.ExpandingStream -> IO (Text, [HP.PrimitiveToken])
expandingStreamAsPrimTokens = loopParser @IO @AppErrorE P.anySingle

-- Command.
expandingStreamAsCommands :: HP.ExpandingStream -> IO (Text, [HP.Command])
expandingStreamAsCommands = loopParser @IO @AppErrorE HP.parseCommand

runApp
  :: HP.ExpandingStream
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

renderStreamUnsetPara :: HP.ExpandingStream -> IO Text
renderStreamUnsetPara s = do
  HList elemSeq <- runApp s extractUnsetParaApp
  pure $ describeLined elemSeq

-- Paragraph boxes.
streamToParaBoxes :: HP.ExpandingStream -> IO (Seq (Box HBox))
streamToParaBoxes s =
  runApp s $ extractUnsetParaApp <&> hListToParaLineBoxes >>= readOnConfState

renderStreamSetPara :: HP.ExpandingStream -> IO Text
renderStreamSetPara s = streamToParaBoxes s <&> describeDoubleLined

-- Pages list.
renderStreamPageList :: HP.ExpandingStream -> IO Text
renderStreamPageList s = do
  MainVModeResult (VList vList) <- runApp s extractMainVList
  pure $ describeLined vList

-- Pages boxes.
streamToPages :: HP.ExpandingStream -> IO (Seq Page, Conf.IntParamVal 'HP.Mag)
streamToPages s = runApp s extractBreakAndSetVList

renderStreamPages :: HP.ExpandingStream -> IO Text
renderStreamPages s = streamToPages s <&> fst <&> describeLined

-- DVI instructions.
streamToSemanticDVI :: HP.ExpandingStream -> IO (Seq Instruction, Conf.IntParamVal 'HP.Mag)
streamToSemanticDVI s = do
  (pages, mag) <- streamToPages s
  pure (pagesToDVI pages, mag)

renderStreamSemanticDVI :: HP.ExpandingStream -> IO Text
renderStreamSemanticDVI s = streamToSemanticDVI s <&> fst <&> describeLined

-- Raw DVI instructions.
streamToRawDVI :: HP.ExpandingStream -> IO (Seq EncodableInstruction)
streamToRawDVI s = do
  (instrs, mag) <- streamToSemanticDVI s
  strEitherToIO $ parseInstructions instrs $ Quantity.unInt $ Conf.unIntParam mag
  where
    strEitherToIO :: Either (Variant '[ByteError, DVIError, PathError]) v -> IO v
    strEitherToIO = \case
      Left err -> panic $ show err
      Right v -> pure v

renderStreamRawDVI :: HP.ExpandingStream -> IO Text
renderStreamRawDVI s = streamToRawDVI s <&> describeLined

-- DVI byte strings.
streamToDVIBytes :: HP.ExpandingStream -> IO ByteString
streamToDVIBytes s = streamToRawDVI s <&> encode
