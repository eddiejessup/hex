module HeX.Config.Ref where

import qualified HeX.Parse.Token                 as T
import           HeXlude

data TeXIntVariable
    = TeXIntParameterVariable T.TeXIntParameter
    | TeXIntRegisterVariable  EightBitInt
    deriving ( Show )
