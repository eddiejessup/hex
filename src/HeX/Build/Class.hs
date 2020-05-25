module Hex.Build.Class where

import           Hexlude

import qualified Hex.Box                     as B
import           Hex.BreakList               (HList(..))
import qualified Hex.BreakList               as BL
import           Hex.Config
import qualified Hex.Lex                     as Lex
import qualified Hex.Parse                   as HP

data EndParaReason
    = EndParaSawEndParaCommand
    | EndParaSawLeaveBox

newtype BuildError = BuildError Text
    deriving stock (Show)

type TeXBuildCtx st e m
  = ( MonadState st m
    , HasType Config st

    , MonadError e m
    , HP.AsTeXParseErrors e
    , AsType HP.ParseError e
    )

class MonadModeIndependentBuild m where

    extractExplicitBox :: B.DesiredLength -> HP.ExplicitBox -> m (B.Box B.BoxContents)

    insertLexToken :: Lex.Token -> m ()

    revertStream :: m ()

    addVElem :: BL.VListElem -> m ()

class MonadHModeBuild m where

    addHElem :: BL.HListElem -> m ()

class MonadVModeBuild m where

  extractPara :: HP.IndentFlag -> m (HList, EndParaReason)
