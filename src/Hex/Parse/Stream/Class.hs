{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.Class where

import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Generics.Product.Typed as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy.Encoding as Tx.L.Enc
import Hex.Config (Config, ConfigError, lookupCS, lookupCatCode)
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Resolve
import Hex.Parse.Parser.Class
import Hexlude hiding (many)
import Path (Abs, File, Path)
import qualified Optics.Cons.Core as O.Cons

class TeXStream s where

  resolutionModeLens :: Lens' s ResolutionMode

  tokenSourceLens :: Lens' s (L.NE.NonEmpty TokenSource)

  lexStateLens :: Lens' s Lex.LexState

  conditionBodyStateLens :: Lens' s [ConditionBodyState]

data TokenSource
  = TokenSource
      { sourcePath :: Maybe (Path Abs File)
      , sourceCharCodes :: BS.L.ByteString
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
      ,     (2, toStrict (Tx.L.Enc.decodeUtf8 (BS.L.take 50 sourceCharCodes)))
      ]
      <> describeNamedRelFoldable1 "lexTokens" (Seq.take 10 sourceLexTokens)

newTokenSource :: Maybe (Path Abs File) -> BS.L.ByteString -> TokenSource
newTokenSource maybePath cs = TokenSource maybePath cs mempty

newtype ExpansionError = ExpansionError Text
  deriving stock Show

newtype ResolutionError = ResolutionError Text
  deriving stock Show

insertLexToken :: TeXStream b => b -> Lex.Token -> b
insertLexToken s t =
  s & tokenSourceLens % neHeadL % G.P.typed @(Seq Lex.Token) %~ O.Cons.cons t

insertLexTokensToStream :: TeXStream s => s -> Seq Lex.Token -> s
insertLexTokensToStream s (ts :|> t) = insertLexTokensToStream (insertLexToken s t) ts
insertLexTokensToStream s Empty = s

type AsTeXParseErrors e
  = ( AsType EvaluationError e
    , AsType ConfigError e
    , AsType ResolutionError e
    )

type TeXParseCtx st e m
  = ( MonadState st m -- Read-only
    , HasType Config st

    , MonadTeXParse m

    , MonadError e m
    , AsTeXParseErrors e
    )

-- Inhibition.
inhibitResolution, enableResolution :: TeXStream s => s -> s
inhibitResolution = resolutionModeLens .~ NotResolving

enableResolution = resolutionModeLens .~ Resolving

fetchResolvedToken
  :: ( MonadError e m
     , AsType ResolutionError e

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
     , TeXStream s
     )
  => s
  -> (Code.CharCode -> Code.CatCode)
  -> (Lex.ControlSequenceLike -> Maybe ResolvedToken)
  -> m (Maybe (Lex.Token, ResolvedToken, s))
extractResolvedToken stream lkpCatCode lkpCS =
  case extractLexToken stream lkpCatCode of
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
     )
  => s
  -> m (Maybe (Lex.Token, s))
fetchLexToken stream = do
  conf <- use (typed @Config)
  let lkpCatCode t = lookupCatCode t conf
  pure $ extractLexToken stream lkpCatCode

extractLexToken :: TeXStream s => s -> (Code.CharCode -> Code.CatCode) -> Maybe (Lex.Token, s)
extractLexToken stream lkpCatCode = do
  (lt, newLexState, newStreamTokenSources) <- extractFromSources (stream ^. tokenSourceLens)
  pure
    ( lt
    , stream &
        tokenSourceLens .~ newStreamTokenSources &
        lexStateLens .~ newLexState
    )
  where
    curLexState = stream ^. lexStateLens

    -- TODO:
    -- [a] -> (a -> Maybe b) -> Maybe (b, [a])
    extractFromSources (curTokSource :| outerTokSources) = case extractFromSource curTokSource of
      Nothing ->
        nonEmpty outerTokSources >>= extractFromSources
      Just (lt, lexState, newCurTokSource) ->
        Just $ seq outerTokSources (lt, lexState, newCurTokSource :| outerTokSources)

    extractFromSource tokSource@TokenSource {sourceCharCodes, sourceLexTokens} = case sourceLexTokens of
      -- If there is a lex token in the buffer, use that.
      fstLexToken :<| laterLexTokens ->
        let newCurTokSource = tokSource {sourceLexTokens = laterLexTokens}
        in Just (fstLexToken, curLexState, newCurTokSource)
      -- If the lex token buffer is empty, extract a token and use it.
      Empty ->
        -- Lex.extractToken lkpCatCode curLexState sourceCharCodes
        undefined lkpCatCode curLexState sourceCharCodes
          <&> \(extractedLexToken, newLexState, newCodes) ->
            (extractedLexToken, newLexState, tokSource {sourceCharCodes = newCodes})
