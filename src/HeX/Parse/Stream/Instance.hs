{-# LANGUAGE UndecidableInstances #-}

module HeX.Parse.Stream.Instance where

import           HeXlude                   hiding (show)

import           Control.Monad.Extra       (mconcatMapM)
import           Control.Monad.Reader      (runReaderT)
import           Data.Char                 (chr)
import qualified Data.List.NonEmpty        as L.NE
import           Data.Map.Strict           ((!?))
import qualified Data.Map.Strict           as Map
import qualified Data.Path                 as D.Path
import           Path                      (Abs, File, Path)
import qualified Path
import qualified Path.IO
import qualified Text.Megaparsec           as P
import           Text.Show

import           HeX.Categorise            (CharCode)
import qualified HeX.Categorise            as Cat
import qualified HeX.Config                as Conf
import           HeX.Evaluate
import qualified HeX.Lex                   as Lex
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

data TokenSource = TokenSource
    { sourcePath      :: Maybe (Path Abs File)
    , sourceCharCodes :: ForwardDirected [] Cat.CharCode
    , sourceLexTokens :: [Lex.Token]
    }
    deriving ( Show )

instance Show ExpandedStream where
    show _ = "ExpandedStream {..}"
newTokenSource :: Maybe (Path Abs File) -> ForwardDirected [] CharCode -> TokenSource
newTokenSource maybePath cs = TokenSource maybePath cs mempty

newExpandStream :: Maybe (Path Abs File) -> ForwardDirected [] Cat.CharCode -> IO ExpandedStream
newExpandStream maybePath cs =
    do
    conf <- Conf.newConfig
    pure ExpandedStream { streamTokenSources = pure (newTokenSource maybePath cs)
                        , lexState      = Lex.LineBegin
                        , expansionMode = Expanding
                        , config        = conf
                        , skipState     = []
                        }

instance TeXParseable ExpandedStream e m => P.Stream ExpandedStream m where
    type Token ExpandedStream = PrimitiveToken

    type Tokens ExpandedStream = [PrimitiveToken]

    -- take1_ :: s -> m (Maybe (Token s, s))
    take1_ stream =
        do
        (_, rt, newStream) <- fetchResolvedToken stream
        case rt of
            -- If it's a primitive token, provide that.
            PrimitiveToken pt ->
                pure (Just (pt, newStream))
            -- If it indicates the start of a syntax command, parse the
            -- remainder of the syntax command.
            SyntaxCommandHeadToken c ->
                do
                (expandStream, lts) <- expandSyntaxCommand newStream c
                P.take1_ $ insertLexTokens expandStream lts

    -- tokensToChunk :: Proxy s -> Proxy m -> [Token s] -> Tokens s
    tokensToChunk _ _ = id

    -- chunkToTokens :: Proxy s -> Proxy m -> Tokens s -> [Token s]
    chunkToTokens _ _ = id

    -- chunkLength :: Proxy s -> Proxy m -> Tokens s -> Int
    chunkLength _ _ = length

    -- If n <= 0, reurn 'Just (mempty, s)', where s is the original stream.
    -- If n > 0 and the stream is empty, return Nothing.
    -- Otherwise, take a chunk of length n, or shorter if the stream is
    -- not long enough, and return the chunk along with the rest of the stream.
    -- takeN_ :: Int -> s -> m (Maybe (Tokens s, s))
    takeN_ n s
        | n <= 0 = pure $ Just (mempty, s)
        | otherwise =
            P.take1_ s >>= \case
                Nothing -> pure Nothing
                Just (t, newS) ->
                    do
                    (tsRest, finalS) <- P.takeN_ (pred n) newS <&> \case
                        Nothing               -> ([],     newS)
                        Just (tsRest, finalS) -> (tsRest, finalS)
                    pure $ Just (t:tsRest, finalS)

    -- Extract chunk while the supplied predicate returns True.
    -- takeWhile_ :: (Token s -> Bool) -> s -> m (Tokens s, s)
    takeWhile_ f s =
        P.take1_ s >>= \case
            Nothing ->
                pure ([], s)
            Just (t, newS) ->
                do
                (tsRest, finalS) <- if f t then P.takeWhile_ f newS
                                           else pure ([], newS)
                pure (t:tsRest, finalS)

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

    getConfig = config

    setConfig c s = s{config = c}

    insertLexToken s t =
        let
            curTokSource@TokenSource{ sourceLexTokens } :| outerStreams = streamTokenSources s
            updatedCurTokSource = curTokSource{ sourceLexTokens = t : sourceLexTokens }
        in
            s{ streamTokenSources = updatedCurTokSource :| outerStreams }

    getConditionBodyState = headMay . skipState

-- Expanding syntax commands.

expandCSName :: ForwardDirected [] CharCode -> [Lex.Token]
expandCSName charToks =
    -- TODO: if control sequence doesn't exist, define one that holds
    -- '\relax'.
    [ Lex.ControlSequenceToken $ Lex.ControlSequence charToks ]

expandString :: Conf.IntParamVal Conf.EscapeChar
             -> Lex.Token
             -> [Lex.Token]
expandString (Conf.IntParamVal escapeCharCode) tok =
    case tok of
        Lex.CharCatToken _ ->
            [ tok ]
        Lex.ControlSequenceToken (Lex.ControlSequence (FDirected s)) ->
            charCodeAsMadeToken <$> addEscapeChar s
  where
    addEscapeChar = if (escapeCharCode < 0) || (escapeCharCode > 255)
        then id
        else (chr escapeCharCode :)

expandMacro :: MonadErrorAnyOf e m '[ExpansionError]
            => MacroContents
            -> Map.Map Digit MacroArgument
            -> m [Lex.Token]
expandMacro MacroContents{replacementTokens = (MacroText replaceToks)} args =
    mconcatMapM renderToken replaceToks
  where
    renderToken = \case
        MacroTextLexToken x
            -> pure [x]
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
expandChangeCase :: (CharCode -> Conf.CaseChangeCode)
                 -> BalancedText
                 -> [Lex.Token]
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

skipToIfDelim :: forall e m. TeXParseable ExpandedStream e m => IfBodyState -> ExpandedStream -> m ExpandedStream
skipToIfDelim blk stream = go stream 1
  where
    go :: ExpandedStream -> Int -> m ExpandedStream
    go _stream n = do
        (_, rt, _stream') <- fetchResolvedToken _stream
        let cont = go _stream'
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
                        pure _stream'
                    | otherwise ->
                        cont $ pred n
            -- If we see an 'else' and are at top condition depth, and our
            -- target block is post-else, we are done skipping tokens. Push a
            -- state to prepare for the later 'end-if'.
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                    | n == 1, IfPreElse <- blk ->
                        pure _stream'{ skipState = IfBodyState IfPostElse : skipState _stream' }
            -- (we ignore 'else's even if it is syntactically wrong, if it's
            -- ourside our block of interest.)
            -- Any other token, just skip and continue unchanged.
            _ ->
                cont n

skipUpToCaseBlock
    :: forall e m
    .  TeXParseable ExpandedStream e m
    => Int
    -> ExpandedStream
    -> m ExpandedStream
skipUpToCaseBlock tgt stream = go stream 0 1
  where
    go :: ExpandedStream -> Int -> Int -> m ExpandedStream
    go _stream cur n
            | n == 1, cur == tgt =
                -- If we are at top condition depth,
                pure _stream{ skipState = CaseBodyState CasePostOr : skipState _stream }
            | otherwise = do
                (_, rt, _stream') <- fetchResolvedToken _stream
                let cont = go _stream'
                case rt of
                    SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
                        cont cur $ succ n
                    SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf ))
                        | n == 1 ->
                            pure _stream'
                        | otherwise ->
                            cont cur $ pred n
                    SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                        | n == 1 ->
                            pure _stream'{ skipState = CaseBodyState CasePostElse : skipState _stream' }
                    SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Or))
                        | n == 1 ->
                            cont (succ cur) n
                    _ ->
                        cont cur n

expandConditionToken :: TeXParseable ExpandedStream e m => ExpandedStream -> ConditionTok -> m ExpandedStream
expandConditionToken strm = \case
    ConditionHeadTok ifTok ->
        do
        (resultStream, condHead) <- runSimpleRunParserT' (conditionHeadParser ifTok) strm
        runReaderT (texEvaluate condHead) (config resultStream) >>= \case
            IfBlockTarget IfPreElse ->
                pure resultStream{ skipState = IfBodyState IfPreElse : skipState resultStream }
            IfBlockTarget IfPostElse ->
                skipToIfDelim IfPreElse resultStream
            CaseBlockTarget tgt ->
                skipUpToCaseBlock tgt resultStream
    ConditionBodyTok delim -> case (delim, skipState strm) of
        -- Shouldn't see any condition token outside a condition block.
        (_, []) ->
            throwM $ ExpansionError $ "Not in a condition body, but saw condition-body token: " <> showT delim
        -- Shouldn't see an 'or' while in an if-condition.
        (Or, IfBodyState _ : _) ->
            throwM $ ExpansionError "In an if-condition, not case-condition, but saw 'or'"
        -- If we see an 'end-if' while in any condition, then pop the
        -- condition.
        (EndIf, _ : condRest) ->
            pure strm{skipState = condRest}
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
        do
        skipStream <- skipToIfDelim IfPostElse strm
        pure skipStream{skipState = condRest}

expandSyntaxCommand
    :: TeXParseable ExpandedStream e m
    => ExpandedStream
    -> SyntaxCommandHeadToken
    -> m (ExpandedStream, [Lex.Token])
expandSyntaxCommand strm = \case
    MacroTok m ->
        runExpandCommand strm (parseMacroArgs m) (expandMacro m)
    ConditionTok ct ->
        expandConditionToken strm ct <&> (, [])
    NumberTok ->
        panic "Not implemented: syntax command NumberTok"
    RomanNumeralTok ->
        panic "Not implemented: syntax command RomanNumeralTok"
    StringTok ->
        do
        let escapeChar = (Conf.IntParamVal . Conf.lookupTeXIntParameter EscapeChar . config) strm
        (postArgStream, lexTok) <- runSimpleRunParserT' parseLexToken strm
        pure (postArgStream, expandString escapeChar lexTok)
    JobNameTok ->
        panic "Not implemented: syntax command JobNameTok"
    FontNameTok ->
        panic "Not implemented: syntax command FontNameTok"
    MeaningTok ->
        panic "Not implemented: syntax command MeaningTok"
    CSNameTok ->
        runExpandCommand strm parseCSNameArgs (expandCSName >>> pure)
    ExpandAfterTok ->
        do
        (argLT, postArgStream) <- liftMaybe (throw EndOfInputError) (fetchLexToken strm)
        (expandedStream, postArgLTs) <- fetchAndExpandToken postArgStream
        -- Prepend the unexpanded token.
        pure (expandedStream, argLT : postArgLTs)
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
        newCodes <- liftIO (D.Path.readPathChars path) <&> FDirected
        let newSource = newTokenSource (Just absPath) newCodes
        pure (resultStream{ streamTokenSources = newSource `L.NE.cons` streamTokenSources }, [])
    EndInputTok ->
        panic "Not implemented: syntax command EndInputTok"
    TheTok ->
        do
        (resultStream, intQuant) <- runSimpleRunParserT' parseInternalQuantity strm
        quantTokens <- (charCodeAsMadeToken <$>) <$> runReaderT (texEvaluate intQuant) (config resultStream)
        pure (resultStream, quantTokens)
    ChangeCaseTok direction ->
        runExpandCommand strm parseGeneralText $ expandChangeCase (\c -> Conf.lookupChangeCaseCode direction c (config strm)) >>> pure
  where
    runExpandCommand inputStream parser f =
        do
        (stream, a) <- runSimpleRunParserT' parser inputStream
        v <- f a
        pure (stream, v)

-- Get the next lex token, and update our stream.
fetchLexToken :: ExpandedStream -> Maybe (Lex.Token, ExpandedStream)
fetchLexToken stream =
    case sourceLexTokens of
        -- If there is a lex token in the buffer, use that.
        (fstLexToken : laterLexTokens) ->
            let
                newCurTokSource = curTokSource{ sourceLexTokens = laterLexTokens }
            in
                Just (fstLexToken, stream{ streamTokenSources = newCurTokSource :| outerStreams })
        -- If the lex token buffer is empty, extract a token and use it.
        [] ->
            let
                lkpCatCode t = Conf.lookupCatCode t (config stream)
            in
                case Lex.extractToken lkpCatCode (lexState stream) sourceCharCodes of
                    Just (fetchedLexToken, newLexState, newCodes) ->
                        let
                            newCurTokSource = curTokSource{ sourceCharCodes = newCodes }
                        in
                            Just (fetchedLexToken, stream{ streamTokenSources = newCurTokSource :| outerStreams
                                                         , lexState = newLexState })
                    Nothing ->
                        do
                        nonEmptyOuterStreams <- L.NE.nonEmpty outerStreams
                        fetchLexToken stream{ streamTokenSources = nonEmptyOuterStreams }
  where
    curTokSource@TokenSource{ sourceCharCodes, sourceLexTokens } :| outerStreams = streamTokenSources stream

fetchResolvedToken
    :: ( MonadErrorAnyOf e m TeXStreamE
       )
    => ExpandedStream
    -> m (Lex.Token, ResolvedToken, ExpandedStream)
fetchResolvedToken stream =
    do
    (lt, newStream) <- liftMaybe (throw EndOfInputError) $ fetchLexToken stream
    let lkp cs = Conf.lookupCS cs $ config newStream
    rt <- liftMaybe (throw $ ExpansionError $ "Could not resolve token:" <> showT lt) $ resolveToken lkp (expansionMode newStream) lt
    pure (lt, rt, newStream)

fetchAndExpandToken
    :: TeXParseable ExpandedStream e m
    => ExpandedStream
    -> m (ExpandedStream, [Lex.Token])
fetchAndExpandToken stream =
    do
    (lt, rt, newStream) <- fetchResolvedToken stream
    case rt of
        PrimitiveToken _ ->
            pure (newStream, [lt])
        SyntaxCommandHeadToken c ->
            expandSyntaxCommand newStream c
