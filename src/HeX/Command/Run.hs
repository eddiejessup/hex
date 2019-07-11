{-# LANGUAGE RankNTypes #-}

module HeX.Command.Run where

import           HeXlude

import           Control.Monad.Except      (runExceptT)
import           Control.Monad.State.Lazy  (evalStateT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.HashMap.Strict       as HMap
import           DVI.Document              (Instruction, parseInstructions)
import           DVI.Encode                (encode)
import           DVI.Instruction           (EncodableInstruction)

import           HeX.Box
import           HeX.BreakList
import           HeX.Categorise
import           HeX.Command.Build
import           HeX.Command.Common
import           HeX.Lex                   (LexState (..), extractToken)
import           HeX.Parse                 (ExpandedStream, ExpansionMode (..),
                                            IndentFlag (..), TeXParser,
                                            TeXStream, anySingle, defaultCSMap,
                                            parseCommand, resolveToken,
                                            runParser)

usableCatLookup :: CharCode -> CatCode
usableCatLookup = catLookup usableCatCodes

-- Cat

runCat :: ForwardDirected [] CharCode -> IO ()
runCat xs = case extractCharCat usableCatLookup xs of
    Just (cc, xs') ->
        print cc >> runCat xs'
    Nothing ->
        pure ()

-- Lex.

runLex :: ForwardDirected [] CharCode -> IO ()
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

runResolved :: ForwardDirected [] CharCode -> IO ()
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

runParseLoop :: Show a => TeXParser ExpandedStream a -> ExpandedStream -> IO ()
runParseLoop p s = runExceptT (runMaybeT (runParser p s)) >>= \case
    Left err ->
        panic $ show err
    Right Nothing ->
        pure ()
    Right (Just (newS, tok)) ->
        print tok >> runExpand newS

runExpand :: ExpandedStream -> IO ()
runExpand = runParseLoop anySingle

-- Command.

runCommand :: ExpandedStream -> IO ()
runCommand = runParseLoop parseCommand

-- Generic.

strEitherToIO :: Either Text v -> IO v
strEitherToIO (Left err) = panic err
strEitherToIO (Right v)  = pure v

runStream :: ExpandedStream -> MonadBuild ExpandedStream a -> IO a
runStream s f = evalStateT (runExceptT $ unMonadBuild f) s >>= strEitherToIO

-- Paragraph list.

extractParaHList :: TeXStream s => MonadBuild s ForwardHList
extractParaHList =
    (\(ParaResult _ hList) -> hList) <$> extractPara Indent

codesToParaList :: ExpandedStream -> IO ForwardHList
codesToParaList s = runStream s extractParaHList

runPara :: ExpandedStream -> IO ()
runPara s = codesToParaList s >>= printLine

-- Paragraph boxes.

codesToParaBoxes :: ExpandedStream -> IO (ForwardDirected Seq (ForwardDirected [] HBoxElem))
codesToParaBoxes s = runStream s (extractParaHList >>= readOnConfState . hListToParaLineBoxes)

runSetPara :: ExpandedStream -> IO ()
runSetPara s = codesToParaBoxes s >>= putStrLn . describeDoubleLined

-- Pages list.

codesToPages :: ExpandedStream -> IO (ForwardDirected Seq Page)
codesToPages s = runStream s extractBreakAndSetVList

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
    -- Who cares, it's for debugging
    let _mag = 1000
    instrs <- codesToDVI s
    strEitherToIO $ parseInstructions instrs _mag

runDVIRaw :: ExpandedStream -> IO ()
runDVIRaw s = codesToDVIRaw s >>= printLine

-- DVI byte strings.

codesToDVIBytes :: ExpandedStream -> IO ByteString
codesToDVIBytes s = codesToDVIRaw s <&> encode
