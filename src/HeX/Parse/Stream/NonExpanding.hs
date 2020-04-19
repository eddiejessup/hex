{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.NonExpanding where

import           Hexlude

import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.Generics.Product.Typed as G.P
import qualified Data.List.NonEmpty        as L.NE
import qualified Data.Sequence             as Seq
import qualified Data.Text as Tx
import           Path                      (Abs, File, Path)
import qualified Text.Megaparsec           as P

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

instance Readable NonExpandingStream where
    describe (NonExpandingStream { nesLexState, nesResolutionMode, nesTokenSources }) =
        "NonExpandingStream["
                    <> "lexState=" <> show nesLexState
            <> ", " <> "resolutionMode=" <> show nesResolutionMode
            <> "\n"
            <> "Token sources:\n" <> Tx.intercalate "\n" (describe <$> (toList nesTokenSources))

newNonExpandingStream :: Maybe (Path Abs File) -> BS.L.ByteString -> NonExpandingStream
newNonExpandingStream maybePath cs =
  NonExpandingStream
    { nesTokenSources = pure (newTokenSource maybePath cs)
    , nesLexState      = Lex.LineBegin
    , nesResolutionMode = Resolving
    }

instance ( MonadError e m
         , AsTeXParseErrors e
         , AsType NonExpandingStreamError e

         , MonadReader st m
         , HasType Conf.Config st
         ) => P.Stream NonExpandingStream m where
    type Token NonExpandingStream = PrimitiveToken

    type Tokens NonExpandingStream = Seq PrimitiveToken

    -- take1_ :: s -> m (Maybe (Token s, s))
    take1_ stream =
        withJust (nesFetchAndExpandToken stream) $ \(_, pt, newStream) ->
            pure $ Just (newStream `seq` pt, newStream)

    -- tokensToChunk :: Proxy s -> Proxy m -> [Token s] -> Tokens s
    tokensToChunk _ _ = Seq.fromList

    -- chunkToTokens :: Proxy s -> Proxy m -> Tokens s -> [Token s]
    chunkToTokens _ _ = toList

    -- chunkLength :: Proxy s -> Proxy m -> Tokens s -> Int
    chunkLength _ _ = length

    -- If n <= 0, return 'Just (mempty, s)', where s is the original stream.
    -- If n > 0 and the stream is empty, return Nothing.
    -- Otherwise, take a chunk of length n, or shorter if the stream is
    -- not long enough, and return the chunk along with the rest of the stream.
    -- takeN_ :: Int -> s -> m (Maybe (Tokens s, s))
    takeN_ = go mempty
      where
        go acc n strm
            | n <= 0 = pure $ Just (acc, strm)
            | otherwise =
                P.take1_ strm >>= \case
                    Nothing -> pure $ case acc of
                        Empty -> Nothing
                        _ -> Just (acc, strm)
                    Just (t, newS) ->
                        go (acc |> t) (pred n) newS

    -- Extract chunk while the supplied predicate returns True.
    -- takeWhile_ :: (Token s -> Bool) -> s -> m (Tokens s, s)
    takeWhile_ f = go mempty
      where
        go acc s = P.take1_ s >>= \case
            Just (t, newS) | f t ->
                go (acc |> t) newS
            _ ->
                pure (acc, s)

    -- showTokens :: Proxy s -> Proxy m -> NonEmpty (Token s) -> String
    showTokens _ _ = show

    -- reachOffset
    --   :: Proxy m
    --   -> Int             -- ^ Offset to reach
    --   -> PosState s      -- ^ Initial 'PosState' to use
    --   -> (SourcePos, String, PosState s) -- ^ (See below)
    reachOffset _ _ _ = undefined

instance TeXStream NonExpandingStream where
    resolutionModeLens = G.P.typed @ResolutionMode

    tokenSourceLens = G.P.typed @(L.NE.NonEmpty TokenSource)

    lexStateLens = G.P.typed @Lex.LexState

    getConditionBodyState = const Nothing

data NonExpandingStreamError
    = SawSyntaxCommandHeadToken SyntaxCommandHeadToken
    deriving stock (Show)

nesFetchAndExpandToken
    :: ( TeXParseable NonExpandingStream st e m
       , AsType NonExpandingStreamError e
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
