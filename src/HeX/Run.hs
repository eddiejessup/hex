{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Run where

import           Prelude                 hiding ( writeFile )

import           Control.Monad.Except           ( ExceptT
                                                , runExceptT )
import           Control.Monad.State.Lazy       ( StateT
                                                , evalStateT
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.HashMap.Strict           as HMap
import           Data.List                      ( intercalate )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Text.Megaparsec               as P

import           DVI.Instruction                ( EncodableInstruction )
import           DVI.Document                   ( parseInstructions )
import           DVI.Encode                     ( encode )

import           HeX.BreakList
import           HeX.Box
import           HeX.Build
import           HeX.Categorise
import           HeX.Lex                        ( LexState(..)
                                                , extractToken
                                                )
import           HeX.Parse                      ( ExpandedStream
                                                , ExpansionMode(..)
                                                , IndentFlag(..)
                                                , defaultCSMap
                                                , extractHModeCommand
                                                , newExpandStream
                                                , resolveToken
                                                )

usableCatLookup :: CharCode -> CatCode
usableCatLookup = catLookup usableCatCodes

-- Cat

runCat :: [CharCode] -> IO ()
runCat _xs = extractAndPrint _xs
  where
    extractAndPrint xs = case extractCharCat usableCatLookup xs of
        Just (cc, xs') ->
            do
            print cc
            extractAndPrint xs'
        Nothing ->
            pure ()

-- Lex.

runLex :: [CharCode] -> IO ()
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

runResolved :: [CharCode] -> IO ()
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

runExpand :: [CharCode] -> IO ()
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

runCommand :: [CharCode] -> IO ()
runCommand xs = newExpandStream xs >>= extractAndPrint
  where
    extractAndPrint estream = case extractHModeCommand estream of
        Right (P.State {P.stateInput = estream'}, com) ->
            do
            print com
            extractAndPrint estream'
        Left (P.ParseErrorBundle ((P.TrivialError _ (Just P.EndOfInput) _) :| []) _) ->
            pure ()
        Left errs ->
            ioError $ userError $ show errs


-- Generic.

strEitherToIO :: Either String v -> IO v
strEitherToIO (Left err) = ioError $ userError $ err
strEitherToIO (Right v)  = pure v

buildEitherToIO :: P.ShowErrorComponent s => Either (BuildError s) b -> IO b
buildEitherToIO (Left (ParseError errBundle)) = ioError $ userError $ P.showErrorComponent errBundle
buildEitherToIO (Left (ConfigError s))        = ioError $ userError $ "Bad semantics: " ++ s
buildEitherToIO (Right v)                     = pure v

codesToSth
    :: P.ShowErrorComponent s
    => [CharCode]
    -> ExceptT (BuildError s) (StateT ExpandedStream IO) a
    -> IO a
codesToSth xs f =
    newExpandStream xs >>= evalStateT (runExceptT f) >>= buildEitherToIO

-- Paragraph list.

codesToParaList :: [CharCode] -> IO [BreakableHListElem]
codesToParaList xs =
    reverse <$> codesToSth xs (extractHList Indent False)

runPara :: [CharCode] -> IO ()
runPara xs = codesToParaList xs >>= putStrLn . intercalate "\n" . fmap show

-- Paragraph boxes.

codesToParaBoxes :: [CharCode] -> IO [[HBoxElem]]
codesToParaBoxes xs =
    reverse <$> codesToSth xs (extractBreakAndSetHList Indent)

runSetPara :: [CharCode] -> IO ()
runSetPara xs =
    codesToParaBoxes xs >>= putStrLn . intercalate "\n\n" . fmap showLine
  where
    showLine = intercalate "\n" . fmap show

-- Pages list.

codesToPages :: [CharCode] -> IO [Page]
codesToPages xs = codesToSth xs extractBreakAndSetVList

printList :: Show a => [a] -> IO ()
printList = putStrLn . intercalate "\n" . fmap show

runPages :: [CharCode] -> IO ()
runPages xs = codesToPages xs >>= printList

-- DVI instructions.

runDVI :: [CharCode] -> IO ()
runDVI xs = pagesToDVI <$> codesToPages xs >>= printList

-- Raw DVI instructions.

codesToDVIRaw :: [CharCode] -> IO [EncodableInstruction]
codesToDVIRaw xs = do
    pages <- codesToPages xs
    -- Who cares, it's for debugging
    let _mag = 1000
        instrs = pagesToDVI pages
    encInstrs <- strEitherToIO $ parseInstructions instrs _mag
    pure $ reverse encInstrs

runDVIRaw :: [CharCode] -> IO ()
runDVIRaw xs = codesToDVIRaw xs >>= printList

-- DVI byte strings.

codesToDVIBytes :: [CharCode] -> IO ByteString
codesToDVIBytes xs = encode <$> codesToDVIRaw xs
