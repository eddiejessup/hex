{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module HeX.Parse.Stream.NonExpanding where

import           HeXlude                   hiding (show)

import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.List.NonEmpty        as L.NE
import qualified Data.Sequence             as Seq
import           Path                      (Abs, File, Path)
import qualified Text.Megaparsec           as P
import           Text.Show

import qualified HeX.Config                as Conf
import qualified HeX.Lex                   as Lex
import           HeX.Parse.Stream.Class
import           HeX.Parse.Stream.Instance
import           HeX.Parse.Token

data NonExpandingStream = NonExpandingStream
    { nesTokenSources :: L.NE.NonEmpty TokenSource
    , nesLexState           :: Lex.LexState
    , nesConfig             :: Conf.Config
    }

instance Show NonExpandingStream where
    show _ = "NonExpandingStream {..}"

newNonExpandingStream :: Maybe (Path Abs File) -> BS.L.ByteString -> IO NonExpandingStream
newNonExpandingStream maybePath cs =
    do
    conf <- Conf.newConfig
    pure NonExpandingStream
        { nesTokenSources = pure (newTokenSource maybePath cs)
        , nesLexState      = Lex.LineBegin
        , nesConfig        = conf
        }

instance ( MonadErrorAnyOf e m TeXStreamE
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
    setExpansion _ = identity

    getConfig = nesConfig

    setConfig c s = s{nesConfig = c}

    insertLexToken s t =
        let
            curTokSource@TokenSource{ sourceLexTokens } :| outerStreams = nesTokenSources s
            updatedCurTokSource = curTokSource{ sourceLexTokens = t :<| sourceLexTokens }
        in
            s{ nesTokenSources = updatedCurTokSource :| outerStreams }

    getConditionBodyState = const Nothing

nesFetchAndExpandToken
    :: MonadErrorAnyOf e m TeXStreamE
    => NonExpandingStream
    -> m (Maybe (Seq Lex.Token, PrimitiveToken, NonExpandingStream))
nesFetchAndExpandToken stream =
    withJust (fetchResolvedToken stream) $ \(lt, rt, newStream) ->
        case rt of
            PrimitiveToken pt ->
                pure $ Just (singleton lt, pt, newStream)
            SyntaxCommandHeadToken c ->
                throwM $ SawSyntaxCommandHeadInNonExpandingStream c
