module Hex.Build.Class where

import           Hexlude

import qualified Hex.Box                     as B
import           Hex.BreakList               (HList(..))
import qualified Hex.BreakList               as BL
import           Hex.Config
import qualified Hex.Lex                     as Lex
import qualified Hex.Parse.TokenParser.Class as HP
import qualified Hex.Resolve.Token as Tok

data EndParaReason
    = EndParaSawEndParaCommand
    | EndParaSawLeaveBox
    deriving stock (Show)

newtype BuildError = BuildError Text
    deriving stock (Show)

type TeXBuildCtx st e m
  = ( MonadState st m
    , HasType Config st

    , MonadError e m
    , AsType HP.ParseError e
    )

class MonadModeIndependentBuild m where

    extractExplicitBox :: B.DesiredLength -> Tok.ExplicitBox -> m (B.Box B.BoxContents)

    insertLexToken :: Lex.Token -> m ()

    revertStream :: m ()

    addVElem :: BL.VListElem -> m ()

class MonadHModeBuild m where

    addHElem :: BL.HListElem -> m ()

class MonadVModeBuild m where

  extractParaList :: Tok.IndentFlag -> m (HList, EndParaReason)
