module Hex.Command.ModeIndependent where

import qualified Hex.Box as B
import Hex.Config
import Hex.Evaluate
import qualified Hex.Parse as HP
import Hexlude

fetchBox
  :: ( MonadState st m
     , HasType Config st
     , MonadError e m
     , AsType ConfigError e
     , AsType EvaluationError e
     )
  => HP.BoxFetchMode
  -> HP.EightBitTeXInt
  -> m (Maybe (B.Box B.BoxContents))
fetchBox fetchMode idx = do
  eIdx <- texEvaluate idx
  fetchedMaybeBox <- gets $ view $ typed @Config % to (lookupBoxRegister eIdx)
  case fetchMode of
    HP.Lookup -> pure ()
    HP.Pop -> modify $ typed @Config %~ delBoxRegister eIdx HP.Local
  pure fetchedMaybeBox
