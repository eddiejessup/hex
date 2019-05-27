module HeX.Parse.Stream where

import           HeXlude

import           Control.Monad.Except     ( Except, runExcept )
import           Control.Monad.Reader     ( ReaderT, runReaderT )
import           Data.Char                ( chr )
import qualified Data.Foldable            as Fold
import qualified Data.List.NonEmpty       as L.NE
import qualified Data.Map.Strict          as Map
import           Data.Map.Strict          ( (!?) )
import qualified Data.Path                as D.Path
import qualified Data.Set                 as Set
import           Path                     (Path, Abs, File)
import qualified Path
import qualified Path.IO
import qualified Text.Megaparsec          as P

import           HeX.Categorise           ( CharCode )
import qualified HeX.Categorise           as Cat
import qualified HeX.Config               as Conf
import           HeX.Evaluate
import qualified HeX.Lex                  as Lex
import           HeX.Parse.Assignment
import           HeX.Parse.AST
import           HeX.Parse.Command
import           HeX.Parse.Condition
import           HeX.Parse.Helpers
import           HeX.Parse.Inhibited
import           HeX.Parse.Resolve
import           HeX.Parse.SyntaxCommand
import           HeX.Parse.Token

import           System.IO.Unsafe         (unsafePerformIO)

data ExpandedStream = ExpandedStream
    { streamTokenSources :: L.NE.NonEmpty TokenSource
    , lexState      :: Lex.LexState
    , expansionMode :: ExpansionMode
    , config        :: Conf.Config
    , skipState     :: [ConditionBodyState]
    }

data TokenSource = TokenSource
    { sourcePath      :: Maybe (Path Abs File)
    , sourceCharCodes :: ForwardDirected [] Cat.CharCode
    , sourceLexTokens :: [Lex.Token]
    }
    deriving ( Show )

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

-- Expanding syntax commands.

type ExpandedStreamError = BuildError (P.ParseErrorBundle ExpandedStream Void)

expandCSName :: ForwardDirected [] CharCode -> Either ExpandedStreamError [Lex.Token]
expandCSName charToks =
    -- TODO: if control sequence doesn't exist, define one that holds
    -- '\relax'.
    Right [ Lex.ControlSequenceToken $ Lex.ControlSequence charToks ]

expandString :: Conf.IntParamVal Conf.EscapeChar
             -> Lex.Token
             -> Either ExpandedStreamError [Lex.Token]
expandString (Conf.IntParamVal escapeCharCode) tok = Right $
    case tok of
        Lex.CharCatToken _ ->
            [ tok ]
        Lex.ControlSequenceToken (Lex.ControlSequence (FDirected s)) ->
            charCodeAsMadeToken <$> addEscapeChar s
  where
    addEscapeChar = if (escapeCharCode < 0) || (escapeCharCode > 255)
        then id
        else (chr escapeCharCode :)

expandMacro :: MacroContents
            -> Map.Map Digit MacroArgument
            -> Either ExpandedStreamError [Lex.Token]
expandMacro MacroContents{replacementTokens = (MacroText replaceToks)} args =
    renderMacroText replaceToks
  where
    renderMacroText []       = Right []
    renderMacroText (t : ts) = do
        rest <- renderMacroText ts
        case t of
            (MacroTextLexToken x)     -> Right (x : rest)
            (MacroTextParamToken dig) -> case args !? dig of
                Nothing -> Left $ ConfigError "No such parameter"
                Just (MacroArgument arg) -> Right $ arg <> rest

-- Change the case of the parsed tokens.
-- Set the character code of each character token to its
-- \uccode or \lccode value, if that value is non-zero.
-- Don't change the category code.
expandChangeCase :: (CharCode -> Conf.CaseChangeCode)
                 -> BalancedText
                 -> Either ExpandedStreamError [Lex.Token]
expandChangeCase lookupChangeCaseCode (BalancedText caseToks) =
    Right $ changeCase <$> caseToks
      where
        changeCase  = \case
            Lex.CharCatToken (Lex.CharCat char cat) -> Lex.CharCatToken $
                Lex.CharCat (switch char) cat
            t -> t

        switch char = case lookupChangeCaseCode char of
            Conf.NoCaseChange       -> char
            Conf.ChangeToCode char' -> char'

skipToIfDelim :: IfBodyState -> ExpandedStream -> Maybe ExpandedStream
skipToIfDelim blk stream = go stream 1
  where
    go :: ExpandedStream -> Int -> Maybe ExpandedStream
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

                    | n == 1 -> Just _stream'
                    | otherwise -> cont $ pred n
            -- If we see an 'else' and are at top condition depth, and our
            -- target block is post-else, we are done skipping tokens. Push a
            -- state to prepare for the later 'end-if'.
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))

                    | n == 1, IfPreElse <- blk ->
                        Just _stream'{ skipState = IfBodyState IfPostElse : skipState _stream' }
            -- (we ignore 'else's even if it is syntactically wrong, if it's
            -- ourside our block of interest.)
            -- Any other token, just skip and continue unchanged.
            _ -> cont n

skipUpToCaseBlock :: Int -> ExpandedStream -> Maybe ExpandedStream
skipUpToCaseBlock tgt stream = go stream 0 1
  where
    go :: ExpandedStream -> Int -> Int -> Maybe ExpandedStream
    go _stream cur n

            | n == 1, cur == tgt =
                -- If we are at top condition depth,
                Just _stream{ skipState = CaseBodyState CasePostOr : skipState _stream }
            | otherwise = do
                (_, rt, _stream') <- fetchResolvedToken _stream
                let cont = go _stream'
                case rt of
                    SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
                        cont cur $ succ n
                    SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf ))
                        | n == 1 -> Just _stream'
                        | otherwise -> cont cur $ pred n
                    SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                        | n == 1 ->
                            Just _stream'{ skipState = CaseBodyState CasePostElse : skipState _stream' }
                    SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Or))
                        | n == 1 -> cont (succ cur) n
                    _ -> cont cur n


runReadT
    :: ExpandedStream
    -> ReaderT Conf.Config (ExceptT Text IO) a
    -> IO (Either Text a)
runReadT s f = runExceptT $ runReaderT f (config s)

runRead
    :: ExpandedStream
    -> ReaderT Conf.Config (Except e) a
    -> Either e a
runRead s f = runExcept $ runReaderT f (config s)


easierRunParser :: SimpParser b c -> b -> (Either (BuildError (ParseErrorBundle b)) c, b)
easierRunParser p s =
    let
        (P.State resultStream _ _, resultOrErr) = easyRunParser p s
    in
        (ParseError `first` resultOrErr, resultStream)

noTokensConfErr :: BuildError s
noTokensConfErr = ConfigError "Ran out of tokens"

expandConditionToken :: ExpandedStream -> ConditionTok -> (Maybe ExpandedStreamError, ExpandedStream)
expandConditionToken strm = \case
    ConditionHeadTok ifTok ->
        case easierRunParser (conditionHeadParser ifTok) strm of
            (Left err, resultStream) ->
                (Just err, resultStream)
            (Right condHead, resultStream) ->
                case runRead resultStream (texEvaluate condHead) of
                    Left err ->
                        (Just $ ConfigError err, resultStream)
                    Right (IfBlockTarget IfPreElse) ->
                        let pushStream = resultStream{ skipState = IfBodyState IfPreElse : skipState resultStream }
                        in (Nothing, pushStream)
                    Right (IfBlockTarget IfPostElse) ->
                        case skipToIfDelim IfPreElse resultStream of
                            Nothing ->
                                (Just noTokensConfErr, resultStream)
                            Just skipStream ->
                                (Nothing, skipStream)
                    Right (CaseBlockTarget tgt) ->
                        case skipUpToCaseBlock tgt resultStream of
                            Nothing ->
                                (Just noTokensConfErr, resultStream)
                            Just skipStream ->
                                (Nothing, skipStream)
    ConditionBodyTok delim -> case (delim, skipState strm) of
        -- Shouldn't see any condition token outside a condition block.
        (_, []) ->
            (Just $ ConfigError $ "Not in a condition body, but saw condition-body token: " <> show delim, strm)
        -- Shouldn't see an 'or' while in an if-condition.
        (Or, IfBodyState _ : _) ->
            (Just $ ConfigError "In an if-condition, not case-condition, but saw 'or'", strm)
        -- If we see an 'end-if' while in any condition, then pop the
        -- condition.
        (EndIf, _ : condRest) ->
            (Nothing, strm{skipState = condRest})
        -- Should not see an 'or' or an 'else' while in a case-condition, while
        -- processing a block started by 'else'. The same goes for seeing an
        -- 'else' while in an if-condition, having already seen an 'else'.
        (Else, IfBodyState IfPostElse : _) ->
            (Just $ ConfigError "Already saw else in this condition-block", strm)
        (Or, CaseBodyState CasePostElse : _) ->
            (Just $ ConfigError "Already saw else in this case-condition, but saw later 'or'", strm)
        (Else, CaseBodyState CasePostElse : _) ->
            (Just $ ConfigError "Already saw else in this condition-block", strm)
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
    skipToEndOfCondition condRest = case skipToIfDelim IfPostElse strm of
        Nothing ->
            (Just noTokensConfErr, strm)
        Just skipStream ->
            (Nothing, skipStream{skipState = condRest})

expandSyntaxCommand
    :: ExpandedStream
    -> SyntaxCommandHeadToken
    -> (Either ExpandedStreamError [Lex.Token], ExpandedStream)
expandSyntaxCommand strm = \case
    MacroTok m ->
        runExpandCommand strm (parseMacroArgs m) (expandMacro m)
    ConditionTok ct ->
        justToEither `first` expandConditionToken strm ct
    NumberTok ->
        panic "Not implemented: syntax command NumberTok"
    RomanNumeralTok ->
        panic "Not implemented: syntax command RomanNumeralTok"
    StringTok ->
        let escapeChar = (Conf.IntParamVal . Conf.lookupTeXIntParameter EscapeChar . config) strm
        in runExpandCommand strm parseLexToken (expandString escapeChar)
    JobNameTok ->
        panic "Not implemented: syntax command JobNameTok"
    FontNameTok ->
        panic "Not implemented: syntax command FontNameTok"
    MeaningTok ->
        panic "Not implemented: syntax command MeaningTok"
    CSNameTok ->
        runExpandCommand strm parseCSNameArgs expandCSName
    ExpandAfterTok ->
        case fetchLexToken strm of
            Nothing -> (Left noTokensConfErr, strm)
            Just (expandAfterArgLT, strm') ->
                case fetchAndExpandToken strm' of
                    Nothing ->
                        (Left noTokensConfErr, strm')
                    Just (result, expandedStrm) ->
                        -- If the result is 'Right v', prepend the unexpanded
                        -- token.
                        ((expandAfterArgLT :) `second` result, expandedStrm)
    NoExpandTok ->
        panic "Not implemented: syntax command NoExpandTok"
    MarkRegisterTok _ ->
        panic "Not implemented: syntax command MarkRegisterTok"
    -- \input ⟨file name⟩:
    -- - Expand to no tokens
    -- - Prepare to read from the specified file before looking at any more
    --   tokens from the current source.
    InputTok ->
        let
            (fileNameOrErr, resultStream@ExpandedStream{ streamTokenSources }) = easierRunParser parseFileName strm
        in
            case fileNameOrErr of
                Left err ->
                    (Left err, resultStream)
                Right (TeXFilePath texPath) ->
                    let
                        newSource = unsafePerformIO $
                            do
                            let extraDirs = case streamTokenSources & (L.NE.head >>> sourcePath) of
                                    Just p -> [Path.parent p]
                                    Nothing -> []

                            absPath <- Path.IO.makeAbsolute texPath
                            newCodes <- runReadT resultStream (Conf.findFilePath (Conf.WithImplicitExtension "tex") extraDirs texPath)
                                >>= \case
                                        Left e -> panic e
                                        Right p -> FDirected <$> D.Path.readPathChars p
                            pure $ newTokenSource (Just absPath) newCodes
                    in
                        (Right [], resultStream{ streamTokenSources = newSource `L.NE.cons` streamTokenSources })
        -- panic "Not implemented: syntax command InputTok"
    EndInputTok ->
        panic "Not implemented: syntax command EndInputTok"
    TheTok ->
        let
            (intQuantOrErr, resultStream) = easierRunParser parseInternalQuantity strm
            quantTokensOrErr = do  -- Either monad.
                intQuant <- intQuantOrErr
                quantChars <- ConfigError `first` runRead resultStream (texEvaluate intQuant)
                pure $ charCodeAsMadeToken <$> quantChars
        in
            (quantTokensOrErr, resultStream)
    ChangeCaseTok direction ->
        runExpandCommand strm parseGeneralText $ expandChangeCase (\c -> Conf.lookupChangeCaseCode direction c (config strm))
  where
    runExpandCommand inputStream parser f =
        (>>= f) `first` easierRunParser parser inputStream

    justToEither Nothing = Right mempty
    justToEither (Just e) = Left e

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

fetchResolvedToken :: ExpandedStream -> Maybe (Lex.Token, ResolvedToken, ExpandedStream)
fetchResolvedToken stream =
    do
    (lt, newStream) <- fetchLexToken stream
    let lkp cs = Conf.lookupCS cs $ config newStream
    pure (lt, resolveToken lkp (expansionMode newStream) lt, newStream)

fetchAndExpandToken :: ExpandedStream -> Maybe (Either ExpandedStreamError [Lex.Token], ExpandedStream)
fetchAndExpandToken stream =
    do
    (lt, rt, newStream) <- fetchResolvedToken stream
    pure $ case rt of
        PrimitiveToken _ ->
            (Right [lt], newStream)
        SyntaxCommandHeadToken c ->
            expandSyntaxCommand newStream c

instance P.Stream ExpandedStream where
    type Token ExpandedStream = PrimitiveToken

    -- 'Tokens' is synonymous with 'chunk' containing 'token's.
    type Tokens ExpandedStream = [PrimitiveToken]

    -- These basically clarify that, for us, a 'tokens' is a list of type
    -- 'token'.
    -- tokenToChunk :: Proxy s -> Token s -> Tokens s
    -- To make a 'token' into a 'tokens', wrap it in a list.
    tokenToChunk Proxy = pure

    -- tokensToChunk :: Proxy s -> [Token s] -> Tokens s
    -- A list of type 'token' is equivalent to a 'tokens', and vice versa.
    tokensToChunk Proxy = id

    -- chunkToTokens :: Proxy s -> Tokens s -> [Token s]
    chunkToTokens Proxy = id

    -- chunkLength :: Proxy s -> Tokens s -> Int
    -- The length of a chunk is the number of elements in it (it's a list).
    chunkLength Proxy = length

    -- chunkEmpty :: Proxy s -> Tokens s -> Bool
    -- A chunk is empty if it has no elements.
    chunkEmpty Proxy = null

    -- take1_ :: s -> Maybe (Token s, s)
    take1_ stream =
        fetchResolvedToken stream >>= \(_, rt, stream') -> case rt of
            -- If it's a primitive token, provide that.
            PrimitiveToken pt ->
                pure (pt, stream')
            -- If it indicates the start of a syntax command, parse the
            -- remainder of the syntax command.
            SyntaxCommandHeadToken c ->
                let (errOrLTs, expandStream) = expandSyntaxCommand stream' c
                in case errOrLTs of
                    Left err ->
                        pure (SubParserError $ showBuildError err, expandStream)
                    Right lts ->
                        P.take1_ $ insertLexTokens expandStream lts

    takeN_ = panic "Not implemented: stream method takeN_"

    takeWhile_ = panic "Not implemented: stream method takeWhile_"

    showTokens Proxy = show

    reachOffset _ _freshState = (freshSourcePos, "", _freshState)

instance Eq ExpandedStream where
    _ == _ = True

instance InhibitableStream ExpandedStream where
    setExpansion mode = P.updateParserState setStateExpansion
      where
        setStateExpansion est@P.State { P.stateInput = es } =
          est { P.stateInput = es {expansionMode = mode } }

    getConfig = config

    setConfig c s = s{config = c}

    insertLexToken s t =
        let
            curTokSource@TokenSource{ sourceLexTokens } :| outerStreams = streamTokenSources s
            updatedCurTokSource = curTokSource{ sourceLexTokens = t : sourceLexTokens }
        in
            s{ streamTokenSources = updatedCurTokSource :| outerStreams }

    getConditionBodyState = headMay . skipState

showBuildError :: BuildError (P.ParseErrorBundle ExpandedStream Void) -> Text
showBuildError = \case
    ParseError errBundle -> showErrorBundle errBundle
    ConfigError errMsg -> errMsg

showErrorBundle :: (Show (P.Token s), Show e) => P.ParseErrorBundle s e -> Text
showErrorBundle (P.ParseErrorBundle errs _) =
    toS $ Fold.concat $ L.NE.intersperse "\n\n" $ showError <$> errs
  where
    showError (P.TrivialError offset (Just (P.Tokens unexpecteds)) expecteds) =
        "Error at " <> show offset <> ".\n"
        <> "Found unexpected tokens: " <> show (L.NE.toList unexpecteds) <> ".\n"
        <> "Expected one of: " <> show (Set.toList expecteds)
    showError (P.TrivialError offset Nothing expecteds) =
        "Error at " <> show offset <> ".\n"
        <> "Found no unexpected tokens.\n"
        <> "Expected one of: " <> show (Set.toList expecteds)
    showError (P.TrivialError offset (Just P.EndOfInput) expecteds) =
        "Error at " <> show offset <> ".\n"
        <> "Found end of input.\n"
        <> "Expected one of: " <> show (Set.toList expecteds)
    showError (P.TrivialError offset (Just (P.Label lab)) expecteds) =
        "Error at " <> show offset <> ".\n"
        <> "Found label: " <> show lab <> ".\n"
        <> "Expected one of: " <> show (Set.toList expecteds)
    showError (P.FancyError offset sth) =
        "Error at " <> show offset <> ".\n"
        <> "Found fancy error: " <> show sth <> ".\n"
