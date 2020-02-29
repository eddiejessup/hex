{-# LANGUAGE RankNTypes #-}

module HeX.Command.Run where

import           HeXlude

import           Control.Monad.Except      (runExceptT)
import qualified Data.HashMap.Strict       as HMap
import           Data.Path                 (PathError)
import           Data.Byte                 (ByteError)
import qualified Data.ByteString.Lazy      as BS.L
import           DVI.Document              (Instruction, parseInstructions)
import           DVI.Encode                (encode)
import           DVI.Instruction           (EncodableInstruction, DVIError)
import qualified Text.Megaparsec           as P

import           TFM                       (TFMError)

import           HeX.Box
import           HeX.BreakList
import           HeX.Categorise
import           HeX.Command.Build
import           HeX.Command.Common
import qualified HeX.Config                as Conf
import qualified HeX.Config.Codes          as Code
import           HeX.Evaluate              (EvaluationError)
import           HeX.Lex                   (LexState (..), extractToken)
import qualified HeX.Parse                 as HP
import qualified HeX.Quantity              as Quantity

type AppError =
    Variant '[ BuildError
             , Conf.ConfigError
             , EvaluationError
             , PathError
             , HP.ExpansionError
             , TFMError
             ]

newtype App a
    = App { unApp :: ExceptT AppError (StateT HP.ExpandedStream IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState HP.ExpandedStream
             , MonadIO
             , MonadError AppError
             )

usableCatLookup :: Code.CharCode -> Code.CatCode
usableCatLookup = Code.catLookup Code.usableCatCodes

printLine :: (Readable a, Foldable t, Functor t) => t a -> IO ()
printLine = putStrLn . describeLined

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

runCat
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
runCat xs = case extractCharCat usableCatLookup xs of
    Just (cc, xs1) ->
        print cc >> runCat xs1
    Nothing ->
        pure ()

-- Lex.

benchLex
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
benchLex = go LineBegin
  where
    go lexState xs =
        case extractToken usableCatLookup lexState xs of
            Just (_, lexState1, xs1) ->
                go lexState1 xs1
            Nothing ->
                pure ()

runLex
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
runLex = go LineBegin
  where
    go lexState xs =
        case extractToken usableCatLookup lexState xs of
            Just (tok, lexState1, xs1) ->
                do
                print tok
                go lexState1 xs1
            Nothing ->
                pure ()

-- Resolve.

runResolved
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
runResolved = go LineBegin
  where
    go lexState xs =
        case extractToken usableCatLookup lexState xs of
            Just (tok, lexState1, xs1) ->
                print (HP.resolveToken lookupCS HP.Expanding tok) >> go lexState1 xs1
            Nothing ->
                pure ()

    lookupCS cs = HMap.lookup cs HP.defaultCSMap

-- Expand.

runParseLoop
    :: ( Show s
       , Show a
       , Show (P.Token s)
       )
    => HP.SimpleParsecT s (ExceptT (Variant HP.TeXStreamE) IO) a
    -> s
    -> IO ()
runParseLoop p = go
  where
    go s = runExceptT (HP.runSimpleRunParserT' p s) >>= \case
        Left err ->
            panic $ show err
        Right (s1, c) ->
            do
            print c
            go s1

runExpand
    :: ( Show s
       , Show (P.Token s)
       , P.Stream s (ExceptT (Variant HP.TeXStreamE) IO)
       )
    => s
    -> IO ()
runExpand = runParseLoop P.anySingle

-- Command.

runCommand
    :: HP.ExpandedStream
    -> IO ()
runCommand = runParseLoop HP.parseCommand

benchTake
    :: HP.ExpandedStream
    -> IO ()
benchTake s = runExceptT (P.take1_ s) >>= \case
    Left (err :: AppError) -> panic $ show err
    Right Nothing -> pure ()
    Right (Just (_, s1)) -> benchTake s1

benchFetchLex
    :: HP.ExpandedStream
    -> IO ()
benchFetchLex s = case HP.fetchLexToken s of
    Nothing -> pure ()
    Just (_, s1) -> benchFetchLex s1

-- Generic.

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

extractParaHList :: App HList
extractParaHList =
    (\(ParaResult _ hList) -> hList) <$> extractPara HP.Indent

runPara :: HP.ExpandedStream -> IO ()
runPara s =
    do
    HList elemSeq <- runApp s extractParaHList
    printLine elemSeq

-- Paragraph boxes.

codesToParaBoxes :: HP.ExpandedStream -> IO (Seq (Box HBox))
codesToParaBoxes s =
    runApp s $ extractParaHList <&> hListToParaLineBoxes >>= readOnConfState

runSetPara :: HP.ExpandedStream -> IO ()
runSetPara s = codesToParaBoxes s >>= putStrLn . describeDoubleLined

-- Pages list.

runPageList :: HP.ExpandedStream -> IO ()
runPageList s =
    do
    MainVModeResult (VList vList) <- runApp s extractMainVList
    printLine vList

-- Pages boxes.

codesToPages :: HP.ExpandedStream -> IO (Seq Page, Conf.IntParamVal 'HP.Mag)
codesToPages s = runApp s extractBreakAndSetVList

runPages :: HP.ExpandedStream -> IO ()
runPages s = codesToPages s <&> fst >>= printLine

-- DVI instructions.

codesToDVI :: HP.ExpandedStream -> IO (Seq Instruction, Conf.IntParamVal 'HP.Mag)
codesToDVI s =
    do
    (pages, mag) <- codesToPages s
    pure (pagesToDVI pages, mag)

runDVI :: HP.ExpandedStream -> IO ()
runDVI s = codesToDVI s <&> fst >>= printLine

-- Raw DVI instructions.

codesToDVIRaw :: HP.ExpandedStream -> IO (Seq EncodableInstruction)
codesToDVIRaw s = do
    (instrs, mag) <- codesToDVI s
    strEitherToIO $ parseInstructions instrs $ Quantity.unInt $ Conf.unIntParam mag
  where
    strEitherToIO :: Either (Variant '[ByteError, DVIError, PathError]) v -> IO v
    strEitherToIO = \case
        Left err -> panic $ show err
        Right v -> pure v

runDVIRaw :: HP.ExpandedStream -> IO ()
runDVIRaw s = codesToDVIRaw s >>= printLine

-- DVI byte strings.

codesToDVIBytes :: HP.ExpandedStream -> IO ByteString
codesToDVIBytes s = codesToDVIRaw s <&> encode
