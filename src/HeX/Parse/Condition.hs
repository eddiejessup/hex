{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Condition where

import           HeXlude

import           HeX.Parse.AST
import           HeX.Parse.Stream.Class
import           HeX.Parse.Quantity
import qualified HeX.Parse.Token     as T

parseRelation :: TeXParser s Ordering
parseRelation = satisfyThen $
    \t -> if
        | matchOtherToken '<' t -> Just LT
        | matchOtherToken '>' t -> Just GT
        | isEquals t -> Just EQ
        | otherwise -> Nothing

conditionHeadParser :: T.ConditionHeadTok -> TeXParser s ConditionHead
conditionHeadParser = \case
    T.IfTeXIntPairTestTok ->
        IfConditionHead <$> (IfTeXIntPairTest <$> parseTeXInt
                             <*> parseRelation
                             <*> parseTeXInt)
    T.IfLengthPairTestTok ->
        IfConditionHead <$> (IfLengthPairTest <$> parseLength
                             <*> parseRelation
                             <*> parseLength)
    T.IfTeXIntOddTok -> IfConditionHead <$> (IfTeXIntOdd <$> parseTeXInt)
    T.IfInModeTok a -> pure $ (IfConditionHead . IfInMode) a
    T.IfTokenAttributesEqualTok attr ->
        IfConditionHead <$> (IfTokenAttributesEqual attr <$> anySingle <*> anySingle)
    T.IfTokensEqualTok ->
        IfConditionHead <$> (IfTokensEqual <$> parseLexToken <*> parseLexToken)
    T.IfBoxRegisterIsTok attr ->
        IfConditionHead <$> (IfBoxRegisterIs attr <$> parseTeXInt)
    T.IfInputEndedTok -> IfConditionHead <$> (IfInputEnded <$> parseTeXInt)
    T.IfConstTok b -> pure $ (IfConditionHead . IfConst) b
    T.CaseTok -> CaseConditionHead <$> parseTeXInt
