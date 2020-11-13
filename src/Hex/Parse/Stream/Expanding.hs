{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.Stream.Expanding where

import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Hex.Lex as Lex
import Hex.Parse.Stream.Class
import Hexlude
import Path (Abs, File, Path)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (cons)
import qualified Optics.Cons as O.Cons
import qualified Hex.Config.Codes as Code
import Data.Conduit (runConduit, yield, (.|), await, ConduitT)
import Hex.Categorise (extractCharCat)

data ExpandingStream
  = ExpandingStream
      { streamTokenSources :: L.NE.NonEmpty TokenSource
      , lexState :: Lex.LexState
      , streamLkpCatCode :: Code.CharCode -> Code.CatCode
      }
  deriving stock (Generic)

-- Lens for the head of a non-empty list.
neHeadL :: Lens' (L.NE.NonEmpty a) a
neHeadL = lens L.NE.head $ \(_ :| xs) x -> x :| xs

tokenSourceL :: Lens' ExpandingStream (NonEmpty TokenSource)
tokenSourceL = G.P.typed @(L.NE.NonEmpty TokenSource)

newExpandStream :: Maybe (Path Abs File) -> BS.ByteString -> (Code.CharCode -> Code.CatCode) -> ExpandingStream
newExpandStream maybePath cs streamLkpCatCode =
  ExpandingStream
    { streamTokenSources = pure (newTokenSource maybePath cs)
    , lexState = Lex.LineBegin
    , streamLkpCatCode
    }

instance Describe ExpandingStream where
    describe ExpandingStream { lexState, streamTokenSources } =
      [ (0, "ExpandingStream")
      ,   (1, "lexState " <> quote (show lexState))
      ]
      <> describeNamedRelFoldable1 "tokenSources" streamTokenSources

instance TeXStream ExpandingStream where

  addTokenSource :: ExpandingStream -> TokenSource -> ExpandingStream
  addTokenSource = pAddTokenSource

  insertLexToken :: ExpandingStream -> Lex.Token -> ExpandingStream
  insertLexToken s t =
    s & tokenSourceL % neHeadL % G.P.typed @(Seq Lex.Token) %~ O.Cons.cons t

  extractLexToken :: (MonadError e m, AsType Lex.LexError e) => ExpandingStream -> m (Maybe (Lex.Token, ExpandingStream))
  extractLexToken = pExtractLexToken

pAddTokenSource :: ExpandingStream -> TokenSource -> ExpandingStream
pAddTokenSource s src = s & tokenSourceL %~ cons src

pExtractLexToken :: forall e m. (MonadError e m, AsType Lex.LexError e) => ExpandingStream -> m (Maybe (Lex.Token, ExpandingStream))
pExtractLexToken stream = do
  (mayA, newLexState) <- extractFromSources (stream ^. field @"streamLkpCatCode") (stream ^. G.P.typed @Lex.LexState) (stream ^. tokenSourceL)
  pure $ case mayA of
    Nothing ->
      Nothing
    Just (lt, newStreamTokenSources) ->
      Just (lt, stream & tokenSourceL .~ newStreamTokenSources & G.P.typed @Lex.LexState .~ newLexState)

extractFromSources :: forall e m. (MonadError e m, AsType Lex.LexError e) => (Code.CharCode -> Code.CatCode) -> Lex.LexState -> NonEmpty TokenSource -> m (Maybe (Lex.Token, NonEmpty TokenSource), Lex.LexState)
extractFromSources lkpCatCode lexState (curTokSource :| outerTokSources) = do
  (mayA, newLexState) <- extractFromSource lkpCatCode lexState curTokSource
  case mayA of
    Nothing -> do
      case nonEmpty outerTokSources of
        Nothing ->
          pure (Nothing, newLexState)
        Just nextTokSources ->
          extractFromSources lkpCatCode newLexState nextTokSources
    Just (lt, newCurTokSource) ->
      pure (Just $ seq outerTokSources (lt, newCurTokSource :| outerTokSources), newLexState)

extractFromSource :: forall e m. (MonadError e m, AsType Lex.LexError e) => (Code.CharCode -> Code.CatCode) -> Lex.LexState -> TokenSource -> m (Maybe (Lex.Token, TokenSource), Lex.LexState)
extractFromSource lkpCatCode lexState tokSource@TokenSource {sourceCharCodes, sourceLexTokens} = case sourceLexTokens of
  -- If there is a lex token in the buffer, use that.
  fstLexToken :<| laterLexTokens -> do
    let newCurTokSource = tokSource {sourceLexTokens = laterLexTokens}
    pure (Just (fstLexToken, newCurTokSource), lexState)
  -- If the lex token buffer is empty, extract a token and use it.
  Empty -> do
    let
      extractOneLexToken :: ConduitT ByteString Void (StateT Lex.LexState m) (Maybe (Lex.Token, ByteString))
      extractOneLexToken =
        extractCharCat lkpCatCode .| Lex.extractToken .| await >>= \case
          Just lt -> await >>= \case
            Just rest -> pure (Just (lt, rest))
            Nothing -> pure (Just (lt, mempty))
          _ -> pure Nothing

      cond :: ConduitT () Void (StateT Lex.LexState m) (Maybe (Lex.Token, ByteString))
      cond = yield sourceCharCodes .| extractOneLexToken

    (mayA, newLexState) <- runStateT (runConduit cond) lexState
    pure $ case mayA of
      Nothing -> (Nothing, newLexState)
      Just (lt, newCodes) ->
        (Just (lt, tokSource {sourceCharCodes = newCodes}), newLexState)
