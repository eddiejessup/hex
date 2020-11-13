{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.Stream.Expanding where

import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Parse.Stream.Class
import Hex.Resolve
import Hexlude
import Path (Abs, File, Path)
import qualified Data.ByteString as BS

data ExpandingStream
  = ExpandingStream
      { streamTokenSources :: L.NE.NonEmpty TokenSource
      , lexState :: Lex.LexState
      , resolutionMode :: ResolutionMode
      , skipState :: [ConditionBodyState]
      }
  deriving stock (Generic)

newExpandStream :: Maybe (Path Abs File) -> BS.ByteString -> ExpandingStream
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
