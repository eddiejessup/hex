{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Condition where

import qualified Text.Megaparsec               as P

import           HeX.Parse.Helpers
import           HeX.Parse.AST
import           HeX.Parse.Quantity
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Inhibited

parseIfIntegerPairTest :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => SimpParser s IfConditionHead
parseIfIntegerPairTest = IfIntegerPairTest <$> parseNumber <*> parseRelation <*> parseNumber

parseRelation :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => SimpParser s Ordering
parseRelation = satisfyThen tokToOrdering
  where
    tokToOrdering t
        | matchOtherToken '<' t = Just LT
        | matchOtherToken '>' t = Just GT
        | isEquals t            = Just EQ
        | otherwise             = Nothing

parseIfLengthPairTest :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => SimpParser s IfConditionHead
parseIfLengthPairTest = IfLengthPairTest <$> parseLength <*> parseRelation <*> parseLength

parseIfIntegerOdd :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => SimpParser s IfConditionHead
parseIfIntegerOdd = IfIntegerOdd <$> parseNumber

parseIfTokenAttributesEqual :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => T.TokenAttribute -> SimpParser s IfConditionHead
parseIfTokenAttributesEqual attr = IfTokenAttributesEqual attr <$> parsePrimitiveToken <*> parsePrimitiveToken
  where
    parsePrimitiveToken = undefined

parseIfTokensEqual :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => SimpParser s IfConditionHead
parseIfTokensEqual = IfTokensEqual <$> parseToken <*> parseToken

parseIfBoxRegisterIs :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => T.BoxRegisterAttribute -> SimpParser s IfConditionHead
parseIfBoxRegisterIs attr = IfBoxRegisterIs attr <$> parseNumber

parseIfInputEnded :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => SimpParser s IfConditionHead
parseIfInputEnded = IfInputEnded <$> parseNumber

conditionHeadParser :: (Inhibitable s, P.Token s ~ T.PrimitiveToken) => T.IfTok -> SimpParser s ConditionHead
conditionHeadParser = \case
    T.IfIntegerPairTestTok           -> IfConditionHead <$> parseIfIntegerPairTest
    T.IfLengthPairTestTok            -> IfConditionHead <$> parseIfLengthPairTest
    T.IfIntegerOddTok                -> IfConditionHead <$> parseIfIntegerOdd
    T.IfInModeTok a                  -> pure $ (IfConditionHead . IfInMode) a
    T.IfTokenAttributesEqualTok attr -> IfConditionHead <$> parseIfTokenAttributesEqual attr
    T.IfTokensEqualTok               -> IfConditionHead <$> parseIfTokensEqual
    T.IfBoxRegisterIsTok attr        -> IfConditionHead <$> parseIfBoxRegisterIs attr
    T.IfInputEndedTok                -> IfConditionHead <$> parseIfInputEnded
    T.IfConstTok b                   -> pure $ (IfConditionHead . IfConst) b
    T.CaseTok                        -> CaseConditionHead <$> parseNumber
