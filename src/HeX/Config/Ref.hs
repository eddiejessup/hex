module HeX.Config.Ref where

import qualified HeX.Parse.Token                 as T
import           HeXlude

data IntegerVariable
    = IntegerParameterVariable T.IntegerParameter
    | IntegerRegisterVariable  EightBitInt
    deriving ( Show )
