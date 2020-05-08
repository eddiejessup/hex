{-# LANGUAGE RankNTypes #-}

module Hex.Parse.Condition where

import           Hexlude

import           Hex.Parse.AST
import           Hex.Parse.Quantity
import           Hex.Parse.Stream.Class
import qualified Hex.Resolve.Token        as T

parseRelation :: MonadTeXParse m => m Ordering
parseRelation = satisfyThen $ \t -> if
    | matchOtherToken '<' t -> Just LT
    | matchOtherToken '>' t -> Just GT
    | matchOtherToken '=' t -> Just EQ
    | otherwise -> Nothing

conditionHeadParser :: TeXParseCtx st e m => T.ConditionHeadTok -> m ConditionHead
conditionHeadParser = \case
    T.IfTeXIntPairTestTok ->
        IfConditionHead <$> (IfTeXIntPairTest <$> parseTeXInt <*> parseRelation <*> parseTeXInt)
    T.IfLengthPairTestTok ->
        IfConditionHead <$> (IfLengthPairTest <$> parseLength <*> parseRelation <*> parseLength)
    T.IfTeXIntOddTok ->
        IfConditionHead <$> (IfTeXIntOdd <$> parseTeXInt)
    T.IfInModeTok a ->
        pure $ (IfConditionHead . IfInMode) a
    T.IfTokenAttributesEqualTok attr ->
        IfConditionHead <$> (IfTokenAttributesEqual attr <$> anySingle <*> anySingle)
    T.IfTokensEqualTok ->
        IfConditionHead <$> (IfTokensEqual <$> parseLexToken <*> parseLexToken)
    T.IfBoxRegisterIsTok attr ->
        IfConditionHead <$> (IfBoxRegisterIs attr <$> parseTeXInt)
    T.IfInputEndedTok ->
        IfConditionHead <$> (IfInputEnded <$> parseTeXInt)
    T.IfConstTok b ->
        pure $ (IfConditionHead . IfConst) b
    T.CaseTok ->
        CaseConditionHead <$> parseTeXInt
