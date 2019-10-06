{-# LANGUAGE RankNTypes #-}

module HeX.Command.Run where

import           HeXlude

import           Control.Monad.Except      (runExceptT)
import           Control.Monad.State.Lazy  (evalStateT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.HashMap.Strict       as HMap
import           Data.Path                 (PathError)
import           Data.Byte                 (ByteError)
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
                                            IndentFlag (..), TeXStream,
                                            ExpansionError,
                                            defaultCSMap,
                                            parseCommand, resolveToken,
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
    = App { unMonadBuild :: MaybeT (ExceptT AppError (StateT ExpandedStream IO)) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState ExpandedStream
             , MonadIO
             , MonadError AppError
             , Alternative
             )

usableCatLookup :: Code.CharCode -> Code.CatCode
usableCatLookup = Code.catLookup Code.usableCatCodes

-- Cat

runCat
    :: ( MonadIO m
       )
    => Seq Code.CharCode
    -> m ()
runCat xs = case extractCharCat usableCatLookup xs of
    Just (cc, xs') ->
        print cc >> runCat xs'
    Nothing ->
        pure ()

-- Lex.

runLex
    :: ( MonadIO m
       )
    => Seq Code.CharCode
    -> m ()
runLex _xs = extractAndPrint (LineBegin, _xs)
  where
    extractAndPrint (lexState, xs) =
        case extractToken usableCatLookup lexState xs of
            Just (tok, lexState', s') ->
                do
                print tok
                extractAndPrint (lexState', s')
            Nothing ->
                pure ()

-- Resolve.

runResolved
    :: ( MonadIO m
       )
    => Seq Code.CharCode
    -> m ()
runResolved _xs = extractAndPrint (LineBegin, _xs)
  where
    extractAndPrint (lexState, xs) =
        case extractToken usableCatLookup lexState xs of
            Just (tok, lexState', s') ->
                do
                print $ resolveToken lookupCS Expanding tok
                extractAndPrint (lexState', s')
            Nothing ->
                pure ()

    lookupCS cs = HMap.lookup cs defaultCSMap

-- Expand.

runParseLoop
    :: ( TeXStream s
       , Show a
       , Show s
       , Show (P.Token s)
       )
    => SimpleParsecT s (ExceptT (Variant TeXStreamE) IO) a
    -> s
    -> IO ()
runParseLoop p s = runExceptT (runSimpleRunParserT' p s) >>= \case
    Left err ->
        panic $ show err
    Right (newS, tok) ->
        print tok >> runParseLoop p newS

runExpand
    :: ( TeXStream s
       , Show s
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

-- Generic.

runApp
    :: ExpandedStream
    -> App a
    -> IO a
runApp s f = evalStateT (runExceptT $ runMaybeT $ unMonadBuild f) s >>= strEitherToIO
  where
    strEitherToIO = \case
        Left err -> panic $ show err
        Right Nothing -> panic "Unexpectedly ran out of input"
        Right (Just v) -> pure v

-- Paragraph list.

extractParaHList :: App HList
extractParaHList =
    (\(ParaResult _ hList) -> hList) <$> extractPara Indent

codesToParaList :: ExpandedStream -> IO HList
codesToParaList s = runApp s extractParaHList

runPara :: ExpandedStream -> IO ()
runPara s =
    do
    HList elemSeq <- codesToParaList s
    printLine elemSeq

-- Paragraph boxes.

codesToParaBoxes :: ExpandedStream -> IO (Seq (Box HBox))
codesToParaBoxes s =
    runApp s $
        do
        hList <- extractParaHList
        readOnConfState (hListToParaLineBoxes hList)


runSetPara :: ExpandedStream -> IO ()
runSetPara s = codesToParaBoxes s >>= putStrLn . describeDoubleLined

-- Pages list.

codesToPages :: ExpandedStream -> IO (Seq Page)
codesToPages s = runApp s extractBreakAndSetVList

printLine :: (Readable a, Foldable t, Functor t) => t a -> IO ()
printLine = putStrLn . describeLined

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
