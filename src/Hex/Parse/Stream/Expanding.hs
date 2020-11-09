{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.Stream.Expanding where

import qualified Optics.Cons.Core as O.Cons
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.ByteString as BS
import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Path
import qualified Data.Sequence as Seq
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Parse.AST
import Hex.Parse.Assignment
import Hex.Parse.Command
import Hex.Parse.Condition
import Hex.Parse.Stream.Class
import Hex.Parse.SyntaxCommand
import qualified Hex.Quantity as Q
import Hex.Resolve
import Hexlude
import Path (Rel, Abs, File, Path)
import qualified Path
import qualified Path.IO

data ExpandingStream
  = ExpandingStream
      { streamTokenSources :: L.NE.NonEmpty TokenSource
      , lexState :: Lex.LexState
      , resolutionMode :: ResolutionMode
      , skipState :: [ConditionBodyState]
      }
  deriving stock (Generic)

newExpandStream :: Maybe (Path Abs File) -> BS.L.ByteString -> ExpandingStream
newExpandStream maybePath cs =
  ExpandingStream
    { streamTokenSources = pure (newTokenSource maybePath cs)
    , lexState = Lex.LineBegin
    , resolutionMode = Resolving
    , skipState = []
    }

instance Describe ExpandingStream where
    describe ExpandingStream { lexState, resolutionMode, skipState, streamTokenSources } =
      [ (0, "ExpandingStream")
      ,   (1, "lexState " <> quote (show lexState))
      ,   (1, "skipState " <> quote (show skipState))
      ,   (1, "resolutionMode " <> quote (show resolutionMode))
      ]
      <> describeNamedRelFoldable1 "tokenSources" streamTokenSources

instance TeXStream ExpandingStream where

  resolutionModeLens = G.P.typed @ResolutionMode

  tokenSourceLens = G.P.typed @(L.NE.NonEmpty TokenSource)

  lexStateLens = G.P.typed @Lex.LexState

  conditionBodyStateLens = G.P.typed @[ConditionBodyState]
