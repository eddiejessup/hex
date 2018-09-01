{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Lexed.Inhibited where

import qualified Data.Char as C

import qualified HeX.Lex as Lex

import HeX.Parse.Helpers

import HeX.Parse.Lexed.Stream

newtype BalancedText =
  BalancedText [Lex.Token]
  deriving (Show, Eq)

parseNestedBraces :: Int -> SimpLexParser [Lex.Token]
parseNestedBraces 0 = return []
parseNestedBraces n = do
  (x, nextN) <- satisfyThen parseNext
  case nextN of
    0 -> return []
    posN -> (x :) <$> parseNestedBraces posN
  where
    parseNext x@(Lex.CharCatToken Lex.CharCat {cat = Lex.BeginGroup}) =
      Just (x, succ n)
    parseNext x@(Lex.CharCatToken Lex.CharCat {cat = Lex.EndGroup}) =
      Just (x, pred n)
    parseNext x = Just (x, n)

parseBalancedText :: SimpLexParser BalancedText
parseBalancedText = BalancedText <$> parseNestedBraces 1

parseCharLike :: SimpLexParser Integer
parseCharLike = fromIntegral . C.ord <$> satisfyThen tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken Lex.CharCat {char = c}) =
      Just c
    tokToCharLike (Lex.ControlSequenceToken (Lex.ControlSequence [c])) =
      Just c
    tokToCharLike _ = Nothing

parseCSName :: SimpLexParser Lex.ControlSequenceLike
parseCSName = satisfyThen tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken Lex.CharCat {cat = Lex.Active, char = c}) =
      Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) = Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing
