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

import           TFM                       (TFMError)

import           HeX.Box
import           HeX.BreakList
import           HeX.Categorise
import           HeX.Command.Build
import           HeX.Command.Common
import           HeX.Config                (ConfigError)
import           HeX.Evaluate              (EvaluationError)
import           HeX.Lex                   (LexState (..), extractToken)
import           HeX.Parse                 (ExpandedStream, ExpansionMode (..),
                                            IndentFlag (..), TeXParser,
                                            StreamTakeError,
                                            anySingle, defaultCSMap,
                                            parseCommand, resolveToken,
                                            runParser)

type AppError =
    Variant '[ BuildError
             , ConfigError
             , EvaluationError
             , PathError
             , StreamTakeError
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
             , MonadPlus
             )

usableCatLookup :: CharCode -> CatCode
usableCatLookup = catLookup usableCatCodes

-- Cat

runCat
    :: ( MonadIO m
       )
    => ForwardDirected [] CharCode
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
    => ForwardDirected [] CharCode
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
    => ForwardDirected [] CharCode
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
    :: ( Show a
       )
    => TeXParser ExpandedStream a
    -> ExpandedStream
    -> IO ()
runParseLoop p s = runExceptT (runMaybeT (runParser p s)) >>= \case
    Left err ->
        panic $ show err
    Right Nothing ->
        pure ()
    Right (Just (newS, tok)) ->
        print tok >> runExpand newS

runExpand
    :: ExpandedStream
    -> IO ()
runExpand = runParseLoop anySingle

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

extractParaHList :: App ForwardHList
extractParaHList =
    (\(ParaResult _ hList) -> hList) <$> extractPara Indent

codesToParaList :: ExpandedStream -> IO ForwardHList
codesToParaList s = runApp s extractParaHList

runPara :: ExpandedStream -> IO ()
runPara s = codesToParaList s >>= printLine

-- Paragraph boxes.

codesToParaBoxes :: ExpandedStream -> IO (ForwardDirected Seq (ForwardDirected [] HBoxElem))
codesToParaBoxes s = runApp s (extractParaHList >>= readOnConfState . hListToParaLineBoxes)

runSetPara :: ExpandedStream -> IO ()
runSetPara s = codesToParaBoxes s >>= putStrLn . describeDoubleLined

-- Pages list.

codesToPages :: ExpandedStream -> IO (ForwardDirected Seq Page)
codesToPages s = runApp s extractBreakAndSetVList

printLine :: (Readable a, Foldable t, Functor t) => t a -> IO ()
printLine = putStrLn . describeLined

runPages :: ExpandedStream -> IO ()
runPages s = codesToPages s >>= printLine

-- DVI instructions.

codesToDVI :: ExpandedStream -> IO (ForwardDirected [] Instruction)
codesToDVI s = codesToPages s <&> pagesToDVI

runDVI :: ExpandedStream -> IO ()
runDVI s = codesToDVI s >>= printLine

-- Raw DVI instructions.

codesToDVIRaw :: ExpandedStream -> IO (ForwardDirected Seq EncodableInstruction)
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
