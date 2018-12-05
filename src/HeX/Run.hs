{-# LANGUAGE LambdaCase #-}

module HeX.Run where

import           Prelude                 hiding ( writeFile )

import           Control.Monad.Trans.State.Lazy
import           Data.ByteString.Lazy           ( ByteString )
import           Data.List                      ( intercalate )
import qualified Text.Megaparsec               as P

import           DVI.Document                   ( parseInstructions )
import           DVI.Encode                     ( encode
                                                , EncodableInstruction
                                                )

import           HeX.BreakList
import           HeX.Box
import           HeX.Box.Draw
import           HeX.Build
import           HeX.Categorise
import           HeX.Lex                        ( extractToken
                                                , LexState(..)
                                                )
import           HeX.Config
import           HeX.Parse.Resolved             ( defaultCSMap
                                                , resolveToken
                                                )
import           HeX.Parse.Expanded

-- Cat

mChop' :: ([a] -> IO (Maybe [a])) -> [a] -> IO ()
mChop' f xs =
  f xs >>= \case
    Nothing -> pure ()
    Just xs'' -> mChop' f xs''

runCat :: [CharCode] -> IO ()
runCat = mChop' (extractAndPrint usableCharToCat)
  where
    extractAndPrint f s =
      case extractCharCat f s of
        Just (cc, s') -> do
          print cc
          pure $ Just s'
        Nothing -> pure Nothing

-- Lex.

chopLex' :: LexState -> [CharCode] -> IO ()
chopLex' ls xs =
  extractAndPrintLex >>= \case
    Nothing -> pure ()
    Just (ls', xs') -> chopLex' ls' xs'
  where
    extractAndPrintLex =
      case extractToken (extractCharCat usableCharToCat) ls xs of
        Just (tok, ls', s') -> do
          print tok
          pure $ Just (ls', s')
        Nothing -> pure Nothing

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
      case extractToken (extractCharCat usableCharToCat) ls xs of
        Just (tok, lexState', s') -> do
          print $ resolveToken defaultCSMap tok
          pure $ Just (lexState', s')
        Nothing -> pure Nothing

runResolved :: [CharCode] -> IO ()
runResolved = chopResolved' LineBegin

-- Expand.

chopExpand' :: ExpandedStream -> IO ()
chopExpand' estream =
  case P.take1_ estream of
    Just (tok, estream') -> do
      print tok
      chopExpand' estream'
    Nothing -> pure ()

runExpand :: [CharCode] -> IO ()
runExpand xs = chopExpand' $ newExpandStream xs defaultCSMap

-- Command.

chopCommand' :: ExpandedStream -> IO ()
chopCommand' estream =
  case extractHModeCommand estream of
    (P.State {P.stateInput = estream'}, Right com) -> do
      print com
      chopCommand' estream'
    (_, Left (P.TrivialError _ (Just P.EndOfInput) _)) -> pure ()
    (_, Left err) -> error $ show err

runCommand :: [CharCode] -> IO ()
runCommand xs = chopCommand' $ newExpandStream xs defaultCSMap

-- Paragraph list.

codesToParaList :: [CharCode] -> IO [BreakableHListElem]
codesToParaList xs = do
  let stream = newExpandStream xs defaultCSMap
  ((para, _), _) <- newConfig >>= runStateT (extractParagraph True stream)
  pure $ reverse para

runPara :: [CharCode] -> IO ()
runPara xs =
  codesToParaList xs >>= putStrLn . intercalate "\n" . fmap show

-- Paragraph boxes.

codesToParaBoxes :: [CharCode] -> IO [[HBoxElem]]
codesToParaBoxes xs = do
  let stream = newExpandStream xs defaultCSMap
  ((para, _), _) <- newConfig >>= runStateT (extractParagraphLineBoxes True stream)

  pure $ reverse para

runSetPara :: [CharCode] -> IO ()
runSetPara xs =
  codesToParaBoxes xs >>= putStrLn . intercalate "\n\n" . fmap showLine
  where
    showLine = intercalate "\n" . fmap show

-- Pages list.

codesToPages :: [CharCode] -> IO ([Page], Config)
codesToPages xs = do
  let stream = newExpandStream xs defaultCSMap
  newConf <- newConfig
  ((pages, _), conf) <- runStateT (extractPages stream) newConf
  pure (pages, conf)

printList :: Show a => [a] -> IO ()
printList = putStrLn . intercalate "\n" . fmap show

runPages :: [CharCode] -> IO ()
runPages xs =
  fst <$> codesToPages xs >>= printList

-- DVI instructions.

runDVI :: [CharCode] -> IO ()
runDVI xs =
  toDVI . fst <$> codesToPages xs >>= printList

-- Raw DVI instructions.

codesToDVIRaw :: [CharCode] -> IO [EncodableInstruction]
codesToDVIRaw xs = do
  (pages, conf) <- codesToPages xs
  let mag = magnification conf
  let instrs = toDVI pages
  case parseInstructions instrs (unMagnification mag) of
    Left err -> ioError $ userError err
    Right encInstrs -> pure $ reverse encInstrs

runDVIRaw :: [CharCode] -> IO ()
runDVIRaw xs = codesToDVIRaw xs >>= printList

-- DVI byte strings.

codesToDVIBytes :: [CharCode] -> IO ByteString
codesToDVIBytes xs = encode <$> codesToDVIRaw xs
