{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.Class where

import qualified Data.Generics.Product.Typed as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Data.Sequence as Seq
import Hex.Config (Config, lookupCS, lookupCatCode)
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Resolve
import Hexlude hiding (many)
import Path (Abs, File, Path)
import qualified Optics.Cons.Core as O.Cons
import Data.Conduit (yield, (.|), await, runConduit, ConduitT)
import Hex.Categorise (extractCharCat)
import qualified Data.ByteString as BS

class TeXStream s where

  resolutionModeLens :: Lens' s ResolutionMode

  tokenSourceLens :: Lens' s (L.NE.NonEmpty TokenSource)

  lexStateLens :: Lens' s Lex.LexState

  conditionBodyStateLens :: Lens' s [ConditionBodyState]

data TokenSource
  = TokenSource
      { sourcePath :: Maybe (Path Abs File)
      , sourceCharCodes :: BS.ByteString
      , sourceLexTokens :: Seq Lex.Token
      }
  deriving stock (Show, Generic)

-- Lens for the head of a non-empty list.
neHeadL :: Lens' (L.NE.NonEmpty a) a
neHeadL = lens L.NE.head $ \(_ :| xs) x -> x :| xs

instance Describe TokenSource where

  describe TokenSource {sourcePath, sourceCharCodes, sourceLexTokens} =
      [ (0, "TokenSource")
      ,   (1, "path " <> quote (show sourcePath))
      ,   (1, "codes")
      ,     (2, decodeUtf8 (BS.take 50 sourceCharCodes))
      ]
      <> describeNamedRelFoldable1 "lexTokens" (Seq.take 10 sourceLexTokens)

newTokenSource :: Maybe (Path Abs File) -> BS.ByteString -> TokenSource
newTokenSource maybePath cs = TokenSource maybePath cs mempty

newtype ExpansionError = ExpansionError Text
  deriving stock Show

insertLexToken :: TeXStream b => b -> Lex.Token -> b
insertLexToken s t =
  s & tokenSourceLens % neHeadL % G.P.typed @(Seq Lex.Token) %~ O.Cons.cons t

insertLexTokensToStream :: TeXStream s => s -> Seq Lex.Token -> s
insertLexTokensToStream s (ts :|> t) = insertLexTokensToStream (insertLexToken s t) ts
insertLexTokensToStream s Empty = s

-- Inhibition.
inhibitResolution, enableResolution :: TeXStream s => s -> s
inhibitResolution = resolutionModeLens .~ NotResolving

enableResolution = resolutionModeLens .~ Resolving

fetchResolvedToken
  :: ( MonadError e m
     , AsType ResolutionError e
     , AsType Lex.LexError e

     , TeXStream s

     , MonadState st m -- Read-only
     , HasType Config st
     )
  => s
  -> m (Maybe (Lex.Token, ResolvedToken, s))
fetchResolvedToken stream = do
  conf <- use $ typed @Config
  let lkpCatCode t = lookupCatCode t conf
  let lkpCS cs = lookupCS cs conf
  extractResolvedToken stream lkpCatCode lkpCS

extractResolvedToken
  :: ( MonadError e m
     , AsType ResolutionError e
     , AsType Lex.LexError e
     , TeXStream s
     )
  => s
  -> (Code.CharCode -> Code.CatCode)
  -> (Lex.ControlSequenceLike -> Maybe ResolvedToken)
  -> m (Maybe (Lex.Token, ResolvedToken, s))
extractResolvedToken stream lkpCatCode lkpCS =
  extractLexToken stream lkpCatCode >>= \case
    Nothing -> pure Nothing
    Just (lt, newStream) -> do
      rt <-
        note
          (injectTyped $ ResolutionError $ "Could not resolve lex token: " <> renderDescribed lt) $
          resolveToken lkpCS (newStream ^. resolutionModeLens) lt
      pure $ Just (lt, rt, newStream)

fetchLexToken
  :: ( TeXStream s

     , MonadState st m -- Read-only
     , HasType Config st

     , MonadError e m
     , AsType Lex.LexError e
     )
  => s
  -> m (Maybe (Lex.Token, s))
fetchLexToken stream = do
  conf <- use (typed @Config)
  let lkpCatCode t = lookupCatCode t conf
  extractLexToken stream lkpCatCode


extractLexToken :: forall s e m. (TeXStream s, MonadError e m, AsType Lex.LexError e) => s -> (Code.CharCode -> Code.CatCode) -> m (Maybe (Lex.Token, s))
extractLexToken stream lkpCatCode = do
  (mayA, newLexState) <- extractFromSources (stream ^. lexStateLens) (stream ^. tokenSourceLens)
  pure $ case mayA of
    Nothing ->
      Nothing
    Just (lt, newStreamTokenSources) ->
      Just (lt, stream & tokenSourceLens .~ newStreamTokenSources & lexStateLens .~ newLexState)
  where
    extractFromSources :: Lex.LexState -> NonEmpty TokenSource -> m (Maybe (Lex.Token, NonEmpty TokenSource), Lex.LexState)
    extractFromSources lexState (curTokSource :| outerTokSources) = do
      (mayA, newLexState) <- extractFromSource lexState curTokSource
      case mayA of
        Nothing -> do
          case nonEmpty outerTokSources of
            Nothing ->
              pure (Nothing, newLexState)
            Just nextTokSources ->
              extractFromSources newLexState nextTokSources
        Just (lt, newCurTokSource) ->
          pure (Just $ seq outerTokSources (lt, newCurTokSource :| outerTokSources), newLexState)

    extractFromSource :: Lex.LexState -> TokenSource -> m (Maybe (Lex.Token, TokenSource), Lex.LexState)
    extractFromSource lexState tokSource@TokenSource {sourceCharCodes, sourceLexTokens} = case sourceLexTokens of
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
