{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Run where

import           Prelude                 hiding ( writeFile )

import           Control.Monad.State.Lazy       ( evalStateT
                                                , StateT
                                                )
import           Control.Monad.Except           ( ExceptT, runExceptT )
import           Data.ByteString.Lazy           ( ByteString )
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
import           HeX.Lex                        ( extractToken
                                                , LexState(..)
                                                )
import           HeX.Parse                      ( defaultCSMap
                                                , resolveToken
                                                , ExpansionMode(..)
                                                , ExpandedStream
                                                , newExpandStream
                                                , extractHModeCommand
                                                , IndentFlag(..)
                                                )

-- Cat

mChop' :: ([a] -> IO (Maybe [a])) -> [a] -> IO ()
mChop' f xs =
    f xs >>= \case
        Nothing -> pure ()
        Just xs'' -> mChop' f xs''

runCat :: [CharCode] -> IO ()
runCat = mChop' (extractAndPrint (catLookup usableCharCatMap))
  where
    extractAndPrint f s = case extractCharCat f s of
        Just (cc, s') ->
            do
            print cc
            pure $ Just s'
        Nothing ->
            pure Nothing

-- Lex.

chopLex' :: LexState -> [CharCode] -> IO ()
chopLex' ls xs =
    extractAndPrintLex >>= \case
        Nothing         -> pure ()
        Just (ls', xs') -> chopLex' ls' xs'
  where
    extractAndPrintLex =
        case extractToken (catLookup usableCharCatMap) ls xs of
            Just (tok, ls', s') ->
                do
                print tok
                pure $ Just (ls', s')
            Nothing ->
                pure Nothing

runLex :: [CharCode] -> IO ()
runLex = chopLex' LineBegin

-- Resolve.

chopResolved' :: LexState -> [CharCode] -> IO ()
chopResolved' ls xs =
    extractAndPrintResolved >>= \case
        Nothing -> pure ()
        Just (ls', xs') -> chopResolved' ls' xs'
  where
      extractAndPrintResolved =
            case extractToken (catLookup usableCharCatMap) ls xs of
                Just (tok, lexState', s') ->
                    do
                    print $ resolveToken defaultCSMap Expanding tok
                    pure $ Just (lexState', s')
                Nothing ->
                    pure Nothing

runResolved :: [CharCode] -> IO ()
runResolved = chopResolved' LineBegin

-- Expand.

chopExpand' :: ExpandedStream -> IO ()
chopExpand' estream =
    case P.take1_ estream of
        Just (tok, estream') ->
            do
            print tok
            chopExpand' estream'
        Nothing ->
            pure ()

runExpand :: [CharCode] -> IO ()
runExpand xs = newExpandStream xs defaultCSMap >>= chopExpand'

-- Command.

chopCommand' :: ExpandedStream -> IO ()
chopCommand' estream =
    case extractHModeCommand estream of
        Right (P.State {P.stateInput = estream'}, com) ->
            do
            print com
            chopCommand' estream'
        Left (P.ParseErrorBundle ((P.TrivialError _ (Just P.EndOfInput) _) :| []) _) ->
            pure ()
        Left errs ->
            ioError $ userError $ show errs

runCommand :: [CharCode] -> IO ()
runCommand xs = newExpandStream xs defaultCSMap >>= chopCommand'

-- Generic.

strEitherToIO :: Either String v -> IO v
strEitherToIO (Left err) = ioError $ userError $ err
strEitherToIO (Right v)  = pure v

buildEitherToIO :: Either BuildError b -> IO b
buildEitherToIO (Left (ParseError errBundle)) = ioError $ userError $ P.showErrorComponent errBundle
buildEitherToIO (Left (ConfigError s))        = ioError $ userError $ "Bad semantics: " ++ s
buildEitherToIO (Right v)                     = pure v

codesToSth
    :: [CharCode]
    -> ExceptT BuildError (StateT ExpandedStream IO) a
    -> IO a
codesToSth xs f =
    do
    stream <- newExpandStream xs defaultCSMap
    evalStateT (runExceptT f) stream >>= buildEitherToIO

-- Paragraph list.

codesToParaList :: [CharCode] -> IO [BreakableHListElem]
codesToParaList xs =
    reverse <$> codesToSth xs (extractParagraph Indent)

runPara :: [CharCode] -> IO ()
runPara xs =
    codesToParaList xs >>= putStrLn . intercalate "\n" . fmap show

-- Paragraph boxes.

codesToParaBoxes :: [CharCode] -> IO [[HBoxElem]]
codesToParaBoxes xs =
    reverse <$> codesToSth xs (extractParagraphLineBoxes Indent)

runSetPara :: [CharCode] -> IO ()
runSetPara xs =
    codesToParaBoxes xs >>= putStrLn . intercalate "\n\n" . fmap showLine
  where
    showLine = intercalate "\n" . fmap show

-- Pages list.

codesToPages :: [CharCode] -> IO [Page]
codesToPages xs = codesToSth xs extractPages

printList :: Show a => [a] -> IO ()
printList = putStrLn . intercalate "\n" . fmap show

runPages :: [CharCode] -> IO ()
runPages xs =
  codesToPages xs >>= printList

-- DVI instructions.

runDVI :: [CharCode] -> IO ()
runDVI xs =
  pagesToDVI <$> codesToPages xs >>= printList

-- Raw DVI instructions.

codesToDVIRaw :: [CharCode] -> IO [EncodableInstruction]
codesToDVIRaw xs = do
  pages <- codesToPages xs
  -- Who cares, it's for debugging
  let _mag = 1000
  let instrs = pagesToDVI pages
  encInstrs <- strEitherToIO $ parseInstructions instrs _mag
  pure $ reverse encInstrs

runDVIRaw :: [CharCode] -> IO ()
runDVIRaw xs = codesToDVIRaw xs >>= printList

-- DVI byte strings.

codesToDVIBytes :: [CharCode] -> IO ByteString
codesToDVIBytes xs = encode <$> codesToDVIRaw xs
