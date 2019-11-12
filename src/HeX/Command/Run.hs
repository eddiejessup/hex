{-# LANGUAGE RankNTypes #-}

module HeX.Command.Run where

import           HeXlude

import           Control.Monad.Except      (runExceptT)
import           Control.Monad.State.Lazy  (evalStateT)
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
import           HeX.Config                (ConfigError)
import qualified HeX.Config.Codes          as Code
import           HeX.Evaluate              (EvaluationError)
import           HeX.Lex                   (LexState (..), extractToken)
import           HeX.Parse                 (ExpandedStream, ExpansionMode (..),
                                            IndentFlag (..),
                                            ExpansionError,
                                            defaultCSMap,
                                            parseCommand, resolveToken, fetchLexToken,
                                            TeXStreamE, runSimpleRunParserT',
                                            SimpleParsecT)

type AppError =
    Variant '[ BuildError
             , ConfigError
             , EvaluationError
             , PathError
             , ExpansionError
             , TFMError
             ]

newtype App a
    = App { unApp :: ExceptT AppError (StateT ExpandedStream IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState ExpandedStream
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
                print (resolveToken lookupCS Expanding tok) >> go lexState1 xs1
            Nothing ->
                pure ()

    lookupCS cs = HMap.lookup cs defaultCSMap

-- Expand.

runParseLoop
    :: ( Show s
       , Show a
       , Show (P.Token s)
       )
    => SimpleParsecT s (ExceptT (Variant TeXStreamE) IO) a
    -> s
    -> IO ()
runParseLoop p = go
  where
    go s = runExceptT (runSimpleRunParserT' p s) >>= \case
        Left err ->
            panic $ show err
            -- pure ()
        Right (s1, c) ->
            do
            print c
            go s1

runExpand
    :: ( Show s
       , Show (P.Token s)
       , P.Stream s (ExceptT (Variant TeXStreamE) IO)
       )
    => s
    -> IO ()
runExpand = runParseLoop P.anySingle

-- Command.

runCommand
    :: ExpandedStream
    -> IO ()
runCommand = runParseLoop parseCommand

benchTake
    :: ExpandedStream
    -> IO ()
benchTake s = runExceptT (P.take1_ s) >>= \case
    Left (err :: AppError) -> panic $ show err
    Right Nothing -> pure ()
    Right (Just (_, s1)) -> benchTake s1

benchFetchLex
    :: ExpandedStream
    -> IO ()
benchFetchLex s = case fetchLexToken s of
    Nothing -> pure ()
    Just (_, s1) -> benchFetchLex s1

-- Generic.

runApp
    :: ExpandedStream
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
    (\(ParaResult _ hList) -> hList) <$> extractPara Indent

runPara :: ExpandedStream -> IO ()
runPara s =
    do
    HList elemSeq <- runApp s extractParaHList
    printLine elemSeq

-- Paragraph boxes.

codesToParaBoxes :: ExpandedStream -> IO (Seq (Box HBox))
codesToParaBoxes s =
    runApp s $ extractParaHList <&> hListToParaLineBoxes >>= readOnConfState

runSetPara :: ExpandedStream -> IO ()
runSetPara s = codesToParaBoxes s >>= putStrLn . describeDoubleLined

-- Pages list.

runPageList :: ExpandedStream -> IO ()
runPageList s =
    do
    MainVModeResult (VList vList) <- runApp s extractMainVList
    printLine vList

-- Pages boxes.

codesToPages :: ExpandedStream -> IO (Seq Page)
codesToPages s = runApp s extractBreakAndSetVList

runPages :: ExpandedStream -> IO ()
runPages s = codesToPages s >>= printLine

-- DVI instructions.

codesToDVI :: ExpandedStream -> IO (Seq Instruction)
codesToDVI s = codesToPages s <&> pagesToDVI

runDVI :: ExpandedStream -> IO ()
runDVI s = codesToDVI s >>= printLine

-- Raw DVI instructions.

codesToDVIRaw :: ExpandedStream -> IO (Seq EncodableInstruction)
codesToDVIRaw s = do
    let _mag = 1000
    instrs <- codesToDVI s
    strEitherToIO $ parseInstructions instrs _mag
  where
    strEitherToIO :: Either (Variant '[ByteError, DVIError, PathError]) v -> IO v
    strEitherToIO = \case
        Left err -> panic $ show err
        Right v -> pure v

runDVIRaw :: ExpandedStream -> IO ()
runDVIRaw s = codesToDVIRaw s >>= printLine

-- DVI byte strings.

codesToDVIBytes :: ExpandedStream -> IO ByteString
codesToDVIBytes s = codesToDVIRaw s <&> encode
