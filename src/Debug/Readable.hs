module Debug.Readable where

import           Prelude
import qualified Protolude as Pr

class Show a => Readable a where
    describe :: a -> Pr.Text
    describe = showT

showT :: Show a => a -> Pr.Text
showT v = Pr.toS $ show v
