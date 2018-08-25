{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Inhibited where

import qualified Text.Megaparsec as P
import qualified Data.Char as C

import qualified HeX.Expand as Expand
import qualified HeX.Lex as Lex
import HeX.Parse.Helpers (skipManySatisfied)
import qualified HeX.Parse.Helpers as PU
import HeX.Parse.Stream (ExpandedStream(..), SimpLexParser, SimpExpandParser)
import qualified HeX.Parse.Common as PC

parseInhibited :: SimpLexParser a -> SimpExpandParser a
parseInhibited p = do
  P.State{stateInput=ExpandedStream lStream csMap} <- P.getParserState
  case PU.easyRunParser p lStream of
    (_, Left _) -> error "ohnoes"
    (P.State lStr pos prc w, Right v) -> do
      P.setParserState (P.State (ExpandedStream lStr csMap) pos prc w)
      return v

parseNestedBraces :: Int -> SimpLexParser [Lex.Token]
parseNestedBraces 0 = return []
parseNestedBraces n = do
  (x, nextN) <- PU.satisfyThen parseNext
  case nextN of
    0 -> return []
    posN -> (x:) <$> parseNestedBraces posN
  where
    parseNext x@(Lex.CharCatToken Lex.CharCat{cat = Lex.BeginGroup}) = Just (x, succ n)
    parseNext x@(Lex.CharCatToken Lex.CharCat{cat = Lex.EndGroup}) = Just (x, pred n)
    parseNext x = Just (x, n)

parseBalancedText :: SimpLexParser Expand.BalancedText
parseBalancedText = Expand.BalancedText <$> parseNestedBraces 1

parseCharLike :: SimpLexParser Integer
parseCharLike = PU.satisfyThen tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken Lex.CharCat{char=c}) = Just $ fromIntegral c
    tokToCharLike (Lex.ControlSequence (Lex.ControlSymbol char)) = Just $ fromIntegral $ C.ord char
    tokToCharLike _ = Nothing

parseCSName :: SimpLexParser Lex.ControlSequenceLike
parseCSName = PU.satisfyThen tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken Lex.CharCat{cat=Lex.Active, char=c}) = Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequence cs) = Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

parseGeneralText :: SimpExpandParser Expand.BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  -- TODO: Maybe other things can act as left braces.
  PU.skipSatisfied PC.isExplicitLeftBrace
  parseInhibited parseBalancedText
  where
    isFillerItem Expand.Relax = True
    isFillerItem t = PC.isSpace t

