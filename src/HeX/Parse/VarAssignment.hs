{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.VarAssignment where

import qualified Text.Megaparsec               as P

import           HeX.Parse.Helpers
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Stream
import           HeX.Parse.Number
import           HeX.Parse.Length
import           HeX.Parse.Glue

-- AST.

data VariableAssignment
    = IntegerVariableAssignment (QuantVariable T.IntegerParameter) Number
    | LengthVariableAssignment (QuantVariable T.LengthParameter) Length
    | GlueVariableAssignment (QuantVariable T.GlueParameter) Glue
    deriving (Show)

data QuantVariable a
    = ParamVar a
    | TokenVar String
    | RegisterVar Number
    deriving (Show)

parseVariableAssignment :: SimpExpandParser VariableAssignment
parseVariableAssignment =
    P.choice [ parseQuantityAssignment parseIntegerVariable parseNumber IntegerVariableAssignment
             , parseQuantityAssignment parseLengthVariable parseLength LengthVariableAssignment
             , parseQuantityAssignment parseGlueVariable parseGlue GlueVariableAssignment
             ]
  where
    parseQuantityAssignment varParser valParser f =
        f <$> varParser <* skipOptionalEquals <*> valParser

parseQuantityVariable
    :: (T.PrimitiveToken -> Maybe p) -- Try to extract a parameter from a token.
    -> (T.PrimitiveToken -> Maybe String) -- Try to extract a short-def token name from a token.
    -> T.PrimitiveToken -- A token signifying the start of a relevant register address.
    -> SimpExpandParser (QuantVariable p)
parseQuantityVariable getParam getTok regHead =
    P.choice [ ParamVar <$> satisfyThen getParam
             , TokenVar <$> satisfyThen getTok
             , RegisterVar <$> (skipSatisfiedEquals regHead >> parseNumber)
             ]

parseIntegerVariable :: SimpExpandParser (QuantVariable T.IntegerParameter)
parseIntegerVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegInt)
  where
    getParam (T.IntParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.TokenVariableTok T.DefInt s) = Just s
    getTok _ = Nothing

parseLengthVariable :: SimpExpandParser (QuantVariable T.LengthParameter)
parseLengthVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegLen)
  where
    getParam (T.LenParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.TokenVariableTok T.DefLen s) = Just s
    getTok _ = Nothing

parseGlueVariable :: SimpExpandParser (QuantVariable T.GlueParameter)
parseGlueVariable =
    parseQuantityVariable getParam getTok (T.RegisterVariableTok T.RegGlue)
  where
    getParam (T.GlueParamVarTok p) = Just p
    getParam _ = Nothing

    getTok (T.TokenVariableTok T.DefGlue s) = Just s
    getTok _ = Nothing
