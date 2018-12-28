{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.VarAssignment where

import qualified Text.Megaparsec               as P

import           HeX.Parse.Helpers
import qualified HeX.Parse.Resolved            as R
import           HeX.Parse.Expanded.Common
import           HeX.Parse.Expanded.Stream
import           HeX.Parse.Expanded.Number
import           HeX.Parse.Expanded.Length

-- AST.

data VariableAssignment
  = IntegerVariableAssignment IntegerVariable Number
  | LengthVariableAssignment LengthVariable Length
  deriving (Show)

data QuantVariable a
  = ParamVar a
  | TokenVar String
  | RegisterVar Number
  deriving (Show)

data IntegerVariable = IntegerVariable (QuantVariable R.IntegerParameter)
  deriving (Show)
data LengthVariable = LengthVariable (QuantVariable R.LengthParameter)
  deriving (Show)

parseVariableAssignment :: SimpExpandParser VariableAssignment
parseVariableAssignment
  = P.choice [ parseQuantityAssignment parseIntegerVariable parseNumber IntegerVariableAssignment
             , parseQuantityAssignment parseLengthVariable parseLength LengthVariableAssignment ]
  where
    parseQuantityAssignment varParser valParser f =
      f <$> varParser <* skipOptionalEquals <*> valParser

parseQuantityVariable
  :: (R.PrimitiveToken -> Maybe p) -- Try to extract a parameter from a token.
  -> (R.PrimitiveToken -> Maybe String) -- Try to extract a short-def token name from a token.
  -> R.PrimitiveToken -- A token signifying the start of a relevant register address.
  -> SimpExpandParser (QuantVariable p)
parseQuantityVariable getParam getTok regHead
  = P.choice [ ParamVar <$> satisfyThen getParam
             , TokenVar <$> satisfyThen getTok
             , RegisterVar <$> (skipSatisfiedEquals regHead >> parseNumber) ]

parseIntegerVariable :: SimpExpandParser IntegerVariable
parseIntegerVariable =
  IntegerVariable <$> parseQuantityVariable getParam getTok (R.RegisterVariable R.RegInt)
  where
    getParam (R.IntParamVar p) = Just p
    getParam _ = Nothing

    getTok (R.TokenVariable R.DefInt s) = Just s
    getTok _ = Nothing

parseLengthVariable :: SimpExpandParser LengthVariable
parseLengthVariable =
  LengthVariable <$> parseQuantityVariable getParam getTok (R.RegisterVariable R.RegLen)
  where
    getParam (R.LenParamVar p) = Just p
    getParam _ = Nothing

    getTok (R.TokenVariable R.DefLen s) = Just s
    getTok _ = Nothing
