{-# LANGUAGE LambdaCase #-}

module HeX.Run where

import Prelude hiding (writeFile)

import Control.Monad.Trans.State.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import qualified Text.Megaparsec as P

import DVI.Document (parseInstructions)
import DVI.Encode (encode, EncodableInstruction)

import HeX.BreakList
import HeX.Box
import HeX.Box.Draw
import HeX.Build
import HeX.Categorise
import HeX.Lex (extractToken, LexState(..))
import HeX.Config
import HeX.Parse.Resolved (defaultCSMap, resolveToken)
import HeX.Parse.Expanded

-- Cat

mChop' :: ([a] -> IO (Maybe [a])) -> [a] -> IO ()
mChop' f xs =
  f xs >>= \case
    Nothing -> return ()
    Just xs'' -> mChop' f xs''

runCat :: [CharCode] -> IO ()
runCat = mChop' (extractAndPrint usableCharToCat)
  where
    extractAndPrint f s =
      case extractCharCat f s of
        Just (cc, s') -> do
          print cc
          return $ Just s'
        Nothing -> return Nothing

-- Lex.

chopLex' :: LexState -> [CharCode] -> IO ()
chopLex' ls xs =
  extractAndPrintLex >>= \case
    Nothing -> return ()
    Just (ls', xs') -> chopLex' ls' xs'
  where
    extractAndPrintLex =
      case extractToken (extractCharCat usableCharToCat) ls xs of
        Just (tok, ls', s') -> do
          print tok
          return $ Just (ls', s')
        Nothing -> return Nothing

runLex :: [CharCode] -> IO ()
runLex = chopLex' LineBegin

-- Resolve.

chopResolved' :: LexState -> [CharCode] -> IO ()
chopResolved' ls xs =
  extractAndPrintResolved >>= \case
    Nothing -> return ()
    Just (ls', xs') -> chopResolved' ls' xs'
  where
    extractAndPrintResolved =
      case extractToken (extractCharCat usableCharToCat) ls xs of
        Just (tok, lexState', s') -> do
          print $ resolveToken defaultCSMap tok
          return $ Just (lexState', s')
        Nothing -> return Nothing

runResolved :: [CharCode] -> IO ()
runResolved = chopResolved' LineBegin

-- Expand.

chopExpand' :: ExpandedStream -> IO ()
chopExpand' estream =
  case P.take1_ estream of
    Just (tok, estream') -> do
      print tok
      chopExpand' estream'
    Nothing -> return ()

runExpand :: [CharCode] -> IO ()
runExpand xs = chopExpand' $ newExpandStream xs defaultCSMap

-- Command.

chopCommand' :: ExpandedStream -> IO ()
chopCommand' estream =
  case extractHModeCommand estream of
    (P.State {P.stateInput = estream'}, Right com) -> do
      print com
      chopCommand' estream'
    (_, Left (P.TrivialError _ (Just P.EndOfInput) _)) -> return ()
    (_, Left err) -> error $ show err

runCommand :: [CharCode] -> IO ()
runCommand xs = chopCommand' $ newExpandStream xs defaultCSMap

-- Paragraph list.

codesToParaList :: [CharCode] -> IO [BreakableHListElem]
codesToParaList xs = do
  let stream = newExpandStream xs defaultCSMap
  ((para, _), _) <- newConfig >>= runStateT (extractParagraph True stream)
  return $ reverse para

runPara :: [CharCode] -> IO ()
runPara xs =
  codesToParaList xs >>= putStrLn . intercalate "\n" . fmap show

-- Paragraph boxes.

codesToParaBoxes :: [CharCode] -> IO [[HBoxElem]]
codesToParaBoxes xs = do
  let stream = newExpandStream xs defaultCSMap
  ((para, _), _) <- newConfig >>= runStateT (extractParagraphLineBoxes True stream)

  return $ reverse para

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
  return (pages, conf)

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
    Right encInstrs -> return $ reverse encInstrs

runDVIRaw :: [CharCode] -> IO ()
runDVIRaw xs = codesToDVIRaw xs >>= printList

-- DVI byte strings.

codesToDVIBytes :: [CharCode] -> IO ByteString
codesToDVIBytes xs = encode <$> codesToDVIRaw xs
