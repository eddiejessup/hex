{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module HeX.Parse.Stream.Instance where

import           HeXlude                   hiding (show)
import qualified HeXlude

import qualified Control.Lens as L
import           Control.Monad.Reader      (runReaderT)
import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.List.NonEmpty        as L.NE
import           Data.Map.Strict           ((!?))
import qualified Data.Map.Strict           as Map
import qualified Data.Sequence             as Seq
import           Path                      (Abs, File, Path)
import qualified Path
import qualified Path.IO
import qualified Text.Megaparsec           as P
import           Text.Show
import qualified Data.Generics.Product.Typed as G.P
import qualified HeX.Config.Codes          as Code
import qualified HeX.Config                as Conf
import           HeX.Evaluate
import qualified HeX.Lex                   as Lex
import qualified HeX.Quantity              as Q
import           HeX.Parse.Assignment
import           HeX.Parse.AST
import           HeX.Parse.Command
import           HeX.Parse.Condition
import           HeX.Parse.Resolve
import           HeX.Parse.Stream.Class
import           HeX.Parse.SyntaxCommand
import           HeX.Parse.Token

data ExpandedStream = ExpandedStream
    { streamTokenSources :: L.NE.NonEmpty TokenSource
    , lexState           :: Lex.LexState
    , expansionMode      :: ExpansionMode
    , config             :: Conf.Config
    , skipState          :: [ConditionBodyState]
    }
    deriving Generic

instance Show ExpandedStream where
    show _ = "ExpandedStream {..}"

newTokenSource :: Maybe (Path Abs File) -> BS.L.ByteString -> TokenSource
newTokenSource maybePath cs = TokenSource maybePath cs mempty

newExpandStream :: Maybe (Path Abs File) -> BS.L.ByteString -> IO ExpandedStream
newExpandStream maybePath cs =
    do
    conf <- Conf.newConfig
    pure ExpandedStream { streamTokenSources = pure (newTokenSource maybePath cs)
                        , lexState      = Lex.LineBegin
                        , expansionMode = Expanding
                        , config        = conf
                        , skipState     = []
                        }

instance ( MonadErrorAnyOf e m TeXStreamE
         , MonadIO m
         ) => P.Stream ExpandedStream m where
    type Token ExpandedStream = PrimitiveToken

    type Tokens ExpandedStream = Seq PrimitiveToken

    -- take1_ :: s -> m (Maybe (Token s, s))
    take1_ stream =
        withJust (fetchAndExpandToken stream) $ \(lts, rt, newStream) ->
            case newStream `seq` rt of
                -- If it's a primitive token, provide that.
                PrimitiveToken pt ->
                    pure $ Just (pt, newStream)
                -- If it indicates the start of a syntax command, parse the
                -- remainder of the syntax command.
                SyntaxCommandHeadToken _ ->
                    P.take1_ $ insertLexTokens newStream lts

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

instance TeXStream ExpandedStream where
    setExpansion mode s = s{ expansionMode = mode }

    configLens = G.P.typed @Conf.Config
    tokenSourceLens = G.P.typed @(L.NE.NonEmpty TokenSource)
    lexStateLens = G.P.typed @Lex.LexState

    insertLexToken s t =
        let
            curTokSource@TokenSource{ sourceLexTokens } :| outerStreams = streamTokenSources s
            updatedCurTokSource = curTokSource{ sourceLexTokens = t :<| sourceLexTokens }
        in
            s{ streamTokenSources = updatedCurTokSource :| outerStreams }

    getConditionBodyState = headMay . skipState

-- Expanding syntax commands.

expandCSName :: Applicative t => Seq Code.CharCode -> t Lex.Token
expandCSName charToks =
    -- TODO: if control sequence doesn't exist, define one that holds
    -- '\relax'.
    pure (Lex.ControlSequenceToken $ Lex.mkControlSequence charToks)

expandString :: Conf.IntParamVal 'EscapeChar
             -> Lex.Token
             -> Seq Lex.Token
expandString (Conf.IntParamVal escapeCharCodeInt) tok =
    case tok of
        Lex.CharCatToken _ ->
            singleton tok
        Lex.ControlSequenceToken Lex.ControlSequence { Lex.csChars } ->
            charCodeAsMadeToken <$> addEscapeChar csChars
  where
    addEscapeChar = case Code.fromTeXInt escapeCharCodeInt of
        Nothing -> id
        Just escapeCharCode ->
            if (escapeCharCodeInt < 0) || (escapeCharCodeInt > 255)
                then id
                else (escapeCharCode :<|)

expandMacro :: MonadErrorAnyOf e m '[ExpansionError]
            => MacroContents
            -> Map.Map Digit MacroArgument
            -> m (Seq Lex.Token)
expandMacro MacroContents{replacementTokens = (MacroText replaceToks)} args =
    mconcatMapM renderToken replaceToks
  where
    mconcatMapM f = fmap fold . mapM f

    renderToken = \case
        MacroTextLexToken x
            -> pure (singleton x)
        MacroTextParamToken dig ->
            case args !? dig of
                Nothing ->
                    throwM $ ExpansionError "No such parameter"
                Just (MacroArgument arg) ->
                    pure arg

-- Change the case of the parsed tokens.
-- Set the character code of each character token to its
-- \uccode or \lccode value, if that value is non-zero.
-- Don't change the category code.
expandChangeCase :: (Code.CharCode -> Conf.CaseChangeCode)
                 -> BalancedText
                 -> Seq Lex.Token
expandChangeCase lookupChangeCaseCode (BalancedText caseToks) =
    changeCase <$> caseToks
      where
        changeCase  = \case
            Lex.CharCatToken (Lex.CharCat char cat) -> Lex.CharCatToken $
                Lex.CharCat (switch char) cat
            t -> t

        switch char = case lookupChangeCaseCode char of
            Conf.NoCaseChange       -> char
            Conf.ChangeToCode char' -> char'

withJust :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
withJust a k =
    a >>= \case
        Nothing -> pure Nothing
        Just x -> k x

skipToIfDelim :: forall e m. TeXParseable ExpandedStream e m => IfBodyState -> ExpandedStream -> m (Maybe ExpandedStream)
skipToIfDelim blk stream = go stream 1
  where
    go :: ExpandedStream -> Int -> m (Maybe ExpandedStream)
    go goStream n =
        withJust (fetchResolvedToken goStream) $ \(_, rt, nextGoStream) ->
            do
            let cont = go nextGoStream
            case rt of
                -- If we see an 'if', increment the condition depth.
                SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
                    cont $ succ n
                -- If we see an 'end-if'...
                -- ...and are at top condition depth, we are finished with the
                -- condition block altogether.
                -- ...otherwise, decrement the condition depth.
                SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf))
                        | n == 1 ->
                            pure $ Just nextGoStream
                        | otherwise ->
                            cont $ pred n
                -- If we see an 'else' and are at top condition depth, and our
                -- target block is post-else, we are done skipping tokens. Push a
                -- state to prepare for the later 'end-if'.
                SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                        | n == 1, IfPreElse <- blk ->
                            pure $ Just nextGoStream{ skipState = IfBodyState IfPostElse : skipState nextGoStream }
                -- (we ignore 'else's even if it is syntactically wrong, if it's
                -- ourside our block of interest.)
                -- Any other token, just skip and continue unchanged.
                _ ->
                    cont n

skipUpToCaseBlock
    :: forall e m
    .  TeXParseable ExpandedStream e m
    => Q.TeXInt
    -> ExpandedStream
    -> m (Maybe ExpandedStream)
skipUpToCaseBlock tgt stream = go stream 0 1
  where
    go :: ExpandedStream -> Q.TeXInt -> Int -> m (Maybe ExpandedStream)
    go _stream cur n
            | n == 1, cur == tgt =
                -- If we are at top condition depth,
                pure $ Just _stream{ skipState = CaseBodyState CasePostOr : skipState _stream }
            | otherwise =
                withJust (fetchResolvedToken _stream) $ \(_, rt, _stream') ->
                    do
                    let cont = go _stream'
                    case rt of
                        SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
                            cont cur $ succ n
                        SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf ))
                            | n == 1 ->
                                pure $ Just _stream'
                            | otherwise ->
                                cont cur $ pred n
                        SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                            | n == 1 ->
                                pure $ Just _stream'{ skipState = CaseBodyState CasePostElse : skipState _stream' }
                        SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Or))
                            | n == 1 ->
                                cont (succ cur) n
                        _ ->
                            cont cur n

expandConditionToken
    :: TeXParseable ExpandedStream e m
    => ExpandedStream
    -> ConditionTok
    -> m (Maybe ExpandedStream)
expandConditionToken strm = \case
    ConditionHeadTok ifTok ->
        do
        (resultStream, condHead) <- runSimpleRunParserT' (conditionHeadParser ifTok) strm
        runReaderT (texEvaluate condHead) (config resultStream) >>= \case
            IfBlockTarget IfPreElse ->
                pure $ Just resultStream{ skipState = IfBodyState IfPreElse : skipState resultStream }
            IfBlockTarget IfPostElse ->
                skipToIfDelim IfPreElse resultStream
            CaseBlockTarget tgt ->
                skipUpToCaseBlock tgt resultStream
    ConditionBodyTok delim -> case (delim, skipState strm) of
        -- Shouldn't see any condition token outside a condition block.
        (_, []) ->
            throwM $ ExpansionError $ "Not in a condition body, but saw condition-body token: " <> HeXlude.show delim
        -- Shouldn't see an 'or' while in an if-condition.
        (Or, IfBodyState _ : _) ->
            throwM $ ExpansionError "In an if-condition, not case-condition, but saw 'or'"
        -- If we see an 'end-if' while in any condition, then pop the
        -- condition.
        (EndIf, _ : condRest) ->
            pure $ Just strm{skipState = condRest}
        -- Should not see an 'or' or an 'else' while in a case-condition, while
        -- processing a block started by 'else'. The same goes for seeing an
        -- 'else' while in an if-condition, having already seen an 'else'.
        (Else, IfBodyState IfPostElse : _) ->
            throwM $ ExpansionError "Already saw 'else' in this condition-block, but saw later 'else'"
        (Or, CaseBodyState CasePostElse : _) ->
            throwM $ ExpansionError "Already saw 'else' in this case-condition, but saw later 'or'"
        (Else, CaseBodyState CasePostElse : _) ->
            throwM $ ExpansionError "Already saw else in this condition-block, but saw later 'else'"
        -- If we see a block delimiter while not-skipping, then we must have
        -- been not-skipping the previous block, so we should skip all
        -- remaining blocks.
        -- This applies if we see,
        --     - an 'or' or an 'else' while in a case-condition, while
        --       processing a block started by 'or'
        --     - an 'else' while in an if-condition, before having seen an 'else'
        (Else, IfBodyState IfPreElse : condRest) ->
            skipToEndOfCondition condRest
        (Or, CaseBodyState CasePostOr : condRest) ->
            skipToEndOfCondition condRest
        (Else, CaseBodyState CasePostOr : condRest) ->
            skipToEndOfCondition condRest
  where
    skipToEndOfCondition condRest =
        withJust (skipToIfDelim IfPostElse strm) $ \skippedStream ->
            pure $ Just skippedStream{skipState = condRest}

expandSyntaxCommand
    :: (TeXParseable ExpandedStream e m, MonadIO m)
    => ExpandedStream
    -> SyntaxCommandHeadToken
    -> m (Maybe (ExpandedStream, Seq Lex.Token))
expandSyntaxCommand strm = \case
    MacroTok m ->
        runExpandCommand strm (parseMacroArgs m) (expandMacro m)
    ConditionTok ct ->
        withJust (expandConditionToken strm ct) $ \newStrm ->
            pure $ Just (newStrm, mempty)
    NumberTok ->
        panic "Not implemented: syntax command NumberTok"
    RomanNumeralTok ->
        panic "Not implemented: syntax command RomanNumeralTok"
    StringTok ->
        do
        let escapeChar = (Conf.IntParamVal . Conf.lookupTeXIntParameter EscapeChar . config) strm
        (postArgStream, lexTok) <- runSimpleRunParserT' parseLexToken strm
        pure $ Just (postArgStream, expandString escapeChar lexTok)
    JobNameTok ->
        panic "Not implemented: syntax command JobNameTok"
    FontNameTok ->
        panic "Not implemented: syntax command FontNameTok"
    MeaningTok ->
        panic "Not implemented: syntax command MeaningTok"
    CSNameTok ->
        runExpandCommand strm parseCSNameArgs (expandCSName >>> pure)
    ExpandAfterTok ->
        case fetchLexToken strm of
            Nothing -> pure Nothing
            Just (argLT, postArgStream) ->
                withJust (fetchAndExpandToken postArgStream) $ \(postArgLTs, _, expandedStream) ->
                    -- Prepend the unexpanded token.
                    pure $ Just (expandedStream, argLT :<| postArgLTs)
    NoExpandTok ->
        panic "Not implemented: syntax command NoExpandTok"
    MarkRegisterTok _ ->
        panic "Not implemented: syntax command MarkRegisterTok"
    -- \input ⟨file name⟩:
    -- - Expand to no tokens
    -- - Prepare to read from the specified file before looking at any more
    --   tokens from the current source.
    InputTok ->
        do
        (resultStream@ExpandedStream{ streamTokenSources }, TeXFilePath texPath) <- runSimpleRunParserT' parseFileName strm
        let extraDirs = case streamTokenSources & (L.NE.head >>> sourcePath) of
                Just p  -> [Path.parent p]
                Nothing -> []
        absPath <- Path.IO.makeAbsolute texPath
        path <- runReaderT (Conf.findFilePath (Conf.WithImplicitExtension "tex") extraDirs texPath) (config resultStream)
        newSource <- newTokenSource (Just absPath) <$> Code.readCharCodes path
        pure $ Just (resultStream{ streamTokenSources = newSource `L.NE.cons` streamTokenSources }, mempty)
    EndInputTok ->
        panic "Not implemented: syntax command EndInputTok"
    TheTok ->
        do
        (resultStream, intQuant) <- runSimpleRunParserT' parseInternalQuantity strm
        quantTokens <- (charCodeAsMadeToken <$>) <$> runReaderT (texEvaluate intQuant) (config resultStream)
        pure $ Just (resultStream, quantTokens)
    ChangeCaseTok direction ->
        runExpandCommand strm parseGeneralText $ expandChangeCase (\c -> Conf.lookupChangeCaseCode direction c (config strm)) >>> pure
  where
    runExpandCommand inputStream parser f =
        do
        (stream, a) <- runSimpleRunParserT' parser inputStream
        v <- f a
        pure $ Just (stream, v)

fetchLexToken :: TeXStream s => s -> Maybe (Lex.Token, s)
fetchLexToken stream =
    do
    (lt, newLexState, newStreamTokenSources) <- fetchFromSources (L.view tokenSourceLens stream)
    pure
        (lt, stream
            & L.set tokenSourceLens newStreamTokenSources
            & L.set lexStateLens newLexState)
  where
    lkpCatCode t = Conf.lookupCatCode t (L.view configLens stream)

    curLexState = L.view lexStateLens stream

    -- TODO:
    -- [a] -> (a -> Maybe b) -> Maybe (b, [a])
    fetchFromSources (curTokSource :| outerTokSources) =
        case fetchFromSource curTokSource of
            Nothing ->
                nonEmpty outerTokSources >>= fetchFromSources
            Just (lt, lexState, newCurTokSource) ->
                Just $ seq outerTokSources (lt, lexState, newCurTokSource :| outerTokSources)

    fetchFromSource tokSource@TokenSource { sourceCharCodes, sourceLexTokens } =
        case sourceLexTokens of
            -- If there is a lex token in the buffer, use that.
            fstLexToken :<| laterLexTokens ->
                let newCurTokSource = tokSource { sourceLexTokens = laterLexTokens }
                in  Just (fstLexToken, curLexState, newCurTokSource)
            -- If the lex token buffer is empty, extract a token and use it.
            Empty ->
                Lex.extractToken lkpCatCode curLexState sourceCharCodes
                <&> \(fetchedLexToken, newLexState, newCodes) ->
                        (fetchedLexToken, newLexState, tokSource { sourceCharCodes = newCodes })

fetchResolvedToken
    :: ( MonadErrorAnyOf e m '[ExpansionError]
       )
    => ExpandedStream
    -> m (Maybe (Lex.Token, ResolvedToken, ExpandedStream))
fetchResolvedToken stream =
    case fetchLexToken stream of
        Nothing -> pure Nothing
        Just (lt, newStream) ->
            do
            let lkp cs = Conf.lookupCS cs $ config newStream
            rt <- note (throw $ ExpansionError $ "Could not resolve token:" <> HeXlude.show lt) $ resolveToken lkp (expansionMode newStream) lt
            pure $ Just (lt, rt, newStream)

fetchAndExpandToken
    :: (TeXParseable ExpandedStream e m, MonadIO m)
    => ExpandedStream
    -> m (Maybe (Seq Lex.Token, ResolvedToken, ExpandedStream))
fetchAndExpandToken stream =
    withJust (fetchResolvedToken stream) $ \(lt, rt, newStream) ->
        case rt of
            PrimitiveToken _ ->
                -- putText $ "$> ---- Got Lex-token: " <> describe lt <> "\n*> ---- Resolved to Primitive-token: " <> describe pt <> "\n\n"
                pure $ Just (singleton lt, rt, newStream)
            SyntaxCommandHeadToken c ->
                withJust (expandSyntaxCommand newStream c) $ \(expandStream, lts) ->
                    -- putText $ "$> ---- Got Lex-token: " <> describe lt <> "\n*> ---- Resolved to Syntax-command-head-token: " <> describe c <> "\n>> ---- Expanded to lex-tokens: " <> describe lts <> "\n\n"
                    pure $ Just (lts, rt, expandStream)
