{-# LANGUAGE RankNTypes #-}

module HeX.Command.Run where

import           HeXlude

import           Control.Monad.Except      (runExceptT)
import           Control.Monad.State.Lazy  (evalStateT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.HashMap.Strict       as HMap
import           Data.Path                 (PathError)
import           Data.Byte                 (ByteError)
import qualified Data.ByteString           as BS
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
    = App { unApp :: MaybeT (ExceptT AppError (StateT ExpandedStream IO)) a }
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

printLine :: (Readable a, Foldable t, Functor t) => t a -> IO ()
printLine = putStrLn . describeLined

-- Cat

-- benchCat
--     :: ( MonadIO m
--        )
--     => Seq Code.CharCode
--     -> m ()
-- benchCat xs = case extractCharCat usableCatLookup xs of
--     Just (_, xs') ->
--         benchCat xs'
--     Nothing ->
--         pure ()

benchCatBSLike
    :: ( MonadIO m
       )
    => Seq Code.CharCode
    -> m ()
benchCatBSLike xs = case extractCharCatBSLike usableCatLookup xs of
    Just (_, xs') ->
        benchCatBSLike xs'
    Nothing ->
        pure ()

benchCatBS
    :: ( MonadIO m
       )
    => BS.ByteString
    -> m ()
benchCatBS xs = case extractCharCatBS Code.usableCatCodes xs of
    Just (_, xs1) ->
        benchCatBS xs1
    Nothing ->
        pure ()

benchCatBSL
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
benchCatBSL xs = case extractCharCatBSL (Code.catLookup Code.usableCatCodes) xs of
    Just (_, xs') ->
        benchCatBSL xs'
    Nothing ->
        pure ()

-- runCat
--     :: ( MonadIO m
--        )
--     => Seq Code.CharCode
--     -> m ()
-- runCat xs = case extractCharCat usableCatLookup xs of
--     Just (cc, xs') ->
--         print cc >> runCat xs'
--     Nothing ->
--         pure ()

-- Lex.

benchLex
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
benchLex xs0 = go (LineBegin, xs0)
  where
    go (lexState, xs) =
        case extractToken usableCatLookup lexState xs of
            Just (_, lexState1, xs1) ->
                go (lexState1, xs1)
            Nothing ->
                pure ()

runLex
    :: ( MonadIO m
       )
    => BS.L.ByteString
    -> m ()
runLex _xs = go (LineBegin, _xs)
  where
    go (lexState, xs) =
        case extractToken usableCatLookup lexState xs of
            Just (tok, lexState1, xs1) ->
                do
                print tok
                go (lexState1, xs1)
            Nothing ->
                pure ()

-- Resolve.

-- runResolved
--     :: ( MonadIO m
--        )
--     => Seq Code.CharCode
--     -> m ()
-- runResolved _xs = extractAndPrint (LineBegin, _xs)
--   where
--     extractAndPrint (lexState, xs) =
--         case extractToken usableCatLookup lexState xs of
--             Just (tok, lexState', s') ->
--                 do
--                 print $ resolveToken lookupCS Expanding tok
--                 extractAndPrint (lexState', s')
--             Nothing ->
--                 pure ()

--     lookupCS cs = HMap.lookup cs defaultCSMap

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
runApp s f = evalStateT (runExceptT $ runMaybeT $ unApp f) s >>= strEitherToIO
  where
    strEitherToIO = \case
        Left err -> panic $ show err
        Right Nothing -> panic "Unexpectedly ran out of input"
        Right (Just v) -> pure v

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
