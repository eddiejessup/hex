module HeX.Command.Run where

import           HeXlude

import           Control.Monad.Except     (runExceptT)
import           Control.Monad.State.Lazy (evalStateT)
import qualified Data.HashMap.Strict      as HMap
import           Data.List.NonEmpty       (NonEmpty (..))
import           DVI.Document             (Instruction, parseInstructions)
import           DVI.Encode               (encode)
import           DVI.Instruction          (EncodableInstruction)
import qualified Text.Megaparsec          as P

import           HeX.Box
import           HeX.BreakList
import           HeX.Categorise
import           HeX.Command.Build
import           HeX.Command.Common
import           HeX.Lex                  (LexState (..), extractToken)
import           HeX.Parse                (ExpandedStream, ExpansionMode (..),
                                           IndentFlag (..), InhibitableStream,
                                           defaultCSMap, extractCommand,
                                           newExpandStream, resolveToken)

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

runExpand :: ForwardDirected [] CharCode -> IO ()
runExpand xs = newExpandStream xs >>= extractAndPrint
  where
    extractAndPrint estream = case P.take1_ estream of
        Just (tok, estream') ->
            do
            print tok
            extractAndPrint estream'
        Nothing ->
            pure ()

-- Command.

runCommand :: ForwardDirected [] CharCode -> IO ()
runCommand xs = newExpandStream xs >>= extractAndPrint
  where
    extractAndPrint estream = case extractCommand estream of
        Right (P.State {P.stateInput = estream'}, com) ->
            do
            print com
            extractAndPrint estream'
        Left (P.ParseErrorBundle ((P.TrivialError _ (Just P.EndOfInput) _) :| []) _) ->
            pure ()
        Left errs ->
            panic $ show errs

-- Generic.

strEitherToIO :: Either Text v -> IO v
strEitherToIO (Left err) = panic $ err
strEitherToIO (Right v)  = pure v

buildEitherToIO :: P.ShowErrorComponent s => Either (BuildError s) b -> IO b
buildEitherToIO (Left (ParseError errBundle)) = panic $ toS $ P.showErrorComponent errBundle
buildEitherToIO (Left (ConfigError s))        = panic $ "Bad semantics: " <> s
buildEitherToIO (Right v)                     = pure v

codesToSth
    :: ForwardDirected [] CharCode
    -> ExceptMonadBuild ExpandedStream a
    -> IO a
codesToSth xs f =
    newExpandStream xs
    >>= evalStateT (unMonadBuild $ runExceptT f)
    >>= buildEitherToIO

-- Paragraph list.

extractParaHList :: InhibitableStream s => ExceptMonadBuild s ForwardHList
extractParaHList =
    (\(ParaResult _ hList) -> hList) <$> extractPara Indent

codesToParaList :: ForwardDirected [] CharCode -> IO ForwardHList
codesToParaList xs =
    codesToSth xs extractParaHList

runPara :: ForwardDirected [] CharCode -> IO ()
runPara xs = codesToParaList xs >>= printLine

-- Paragraph boxes.

codesToParaBoxes :: ForwardDirected [] CharCode -> IO (ForwardDirected Seq (ForwardDirected [] HBoxElem))
codesToParaBoxes xs =
    codesToSth xs (extractParaHList >>= readOnConfState . hListToParaLineBoxes)

runSetPara :: ForwardDirected [] CharCode -> IO ()
runSetPara xs =
    codesToParaBoxes xs >>= putStrLn . describeDoubleLined

-- Pages list.

codesToPages :: ForwardDirected [] CharCode -> IO (ForwardDirected Seq Page)
codesToPages xs = codesToSth xs extractBreakAndSetVList

printLine :: (Readable a, Foldable t, Functor t) => t a -> IO ()
printLine = putStrLn . describeLined

runPages :: ForwardDirected [] CharCode -> IO ()
runPages xs = codesToPages xs >>= printLine

-- DVI instructions.

codesToDVI :: ForwardDirected [] CharCode -> IO (ForwardDirected [] Instruction)
codesToDVI xs = codesToPages xs <&> pagesToDVI

runDVI :: ForwardDirected [] CharCode -> IO ()
runDVI xs = codesToDVI xs >>= printLine

-- Raw DVI instructions.

codesToDVIRaw :: ForwardDirected [] CharCode -> IO (ForwardDirected Seq EncodableInstruction)
codesToDVIRaw xs = do
    -- Who cares, it's for debugging
    let _mag = 1000
    instrs <- codesToDVI xs
    strEitherToIO $ parseInstructions instrs _mag

runDVIRaw :: ForwardDirected [] CharCode -> IO ()
runDVIRaw xs = codesToDVIRaw xs >>= printLine

-- DVI byte strings.

codesToDVIBytes :: ForwardDirected [] CharCode -> IO ByteString
codesToDVIBytes xs = encode <$> codesToDVIRaw xs