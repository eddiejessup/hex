{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Condition where

import qualified Text.Megaparsec               as P

import           HeX.Parse.Helpers
import           HeX.Parse.AST
import           HeX.Parse.Quantity
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Inhibited

parseRelation :: InhibitableStream s => SimpParser s Ordering
parseRelation = satisfyThen $ \t -> if
    | matchOtherToken '<' t -> Just LT
    | matchOtherToken '>' t -> Just GT
    | isEquals t            -> Just EQ
    | otherwise             -> Nothing

conditionHeadParser :: InhibitableStream s => T.ConditionHeadTok -> SimpParser s ConditionHead
conditionHeadParser = \case
    T.IfIntegerPairTestTok ->
        IfConditionHead <$> (IfIntegerPairTest <$> parseNumber <*> parseRelation <*> parseNumber)
    T.IfLengthPairTestTok ->
        IfConditionHead <$> (IfLengthPairTest <$> parseLength <*> parseRelation <*> parseLength)
    T.IfIntegerOddTok ->
        IfConditionHead <$> (IfIntegerOdd <$> parseNumber)
    T.IfInModeTok a ->
        pure $ (IfConditionHead . IfInMode) a
    T.IfTokenAttributesEqualTok attr ->
        IfConditionHead <$> (IfTokenAttributesEqual attr <$> P.anySingle <*> P.anySingle)
    T.IfTokensEqualTok ->
        IfConditionHead <$> (IfTokensEqual <$> parseLexToken <*> parseLexToken)
    T.IfBoxRegisterIsTok attr ->
        IfConditionHead <$> (IfBoxRegisterIs attr <$> parseNumber)
    T.IfInputEndedTok ->
        IfConditionHead <$> (IfInputEnded <$> parseNumber)
    T.IfConstTok b ->
        pure $ (IfConditionHead . IfConst) b
    T.CaseTok ->
        CaseConditionHead <$> parseNumber
