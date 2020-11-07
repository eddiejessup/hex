{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.NonExpanding where

import           Hexlude

import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.Generics.Product.Typed as G.P
import qualified Data.List.NonEmpty        as L.NE
import           Path                      (Abs, File, Path)

import qualified Hex.Config                as Conf
import qualified Hex.Lex                   as Lex
import           Hex.Resolve
import           Hex.Parse.Stream.Class

data NonExpandingStream = NonExpandingStream
    { nesTokenSources :: L.NE.NonEmpty TokenSource
    , nesResolutionMode  :: ResolutionMode
    , nesLexState     :: Lex.LexState
    }
    deriving stock (Generic)

instance Describe NonExpandingStream where
    describe NonExpandingStream { nesLexState, nesResolutionMode, nesTokenSources } =
      [ (0, "NonExpandingStream")
      ,   (1, "lexState " <> quote (show nesLexState))
      ,   (1, "resolutionMode " <> quote (show nesResolutionMode))
      ]
      <> describeNamedRelFoldable1 "tokenSources" nesTokenSources

newNonExpandingStream :: Maybe (Path Abs File) -> BS.L.ByteString -> NonExpandingStream
newNonExpandingStream maybePath cs =
  NonExpandingStream
    { nesTokenSources = pure (newTokenSource maybePath cs)
    , nesLexState      = Lex.LineBegin
    , nesResolutionMode = Resolving
    }

instance TeXStream NonExpandingStream where
    resolutionModeLens = G.P.typed @ResolutionMode

    tokenSourceLens = G.P.typed @(L.NE.NonEmpty TokenSource)

    lexStateLens = G.P.typed @Lex.LexState

    conditionBodyStateLens = lens (const []) const

newtype NonExpandingStreamError
    = SawSyntaxCommandHeadToken SyntaxCommandHeadToken
    deriving stock (Show)

nesFetchAndExpandToken
    :: ( MonadError e m

       , MonadState st m
       , HasType Conf.Config st

       , AsType NonExpandingStreamError e
       , AsTeXParseErrors e
       )
    => NonExpandingStream
    -> m (Maybe (Seq Lex.Token, PrimitiveToken, NonExpandingStream))
nesFetchAndExpandToken stream =
    withJust (fetchResolvedToken stream) $ \(lt, rt, newStream) ->
        case rt of
            PrimitiveToken pt ->
                pure $ Just (singleton lt, pt, newStream)
            SyntaxCommandHeadToken c ->
                throwError $ injectTyped $ SawSyntaxCommandHeadToken c
