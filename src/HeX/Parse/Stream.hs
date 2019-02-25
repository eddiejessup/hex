{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Stream where

import           Control.Monad.Except           ( Except
                                                , runExcept )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT
                                                , gets
                                                , modify
                                                , runStateT
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Data.Char                      ( chr )
import           Data.Proxy
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!?) )
import qualified Data.Set                      as Set
import qualified Data.List.NonEmpty            as NE
import qualified Data.Foldable                 as Fold
import qualified Text.Megaparsec               as P

import           HeX.Type
import qualified HeX.Categorise                as Cat
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Config                    as Conf
import           HeX.Parse.Helpers
import           HeX.Parse.Token
import           HeX.Parse.Command
import           HeX.Parse.Condition
import           HeX.Parse.Inhibited
import           HeX.Parse.SyntaxCommand
import           HeX.Parse.Resolve

data ExpandedStream = ExpandedStream
    { codes         :: [Cat.CharCode]
    , lexTokens     :: [Lex.Token]
    , lexState      :: Lex.LexState
    , expansionMode :: ExpansionMode
    , config        :: Conf.Config
    , skipState     :: [ConditionBodyState]
    } deriving (Show)

runConfState :: MonadState ExpandedStream m => StateT Conf.Config m a -> m a
runConfState f =
    do
    conf <- gets config
    (v, conf') <- runStateT f conf
    modify (\stream -> stream{config=conf'})
    pure v

newExpandStream :: [Cat.CharCode] -> IO ExpandedStream
newExpandStream cs =
    do
    conf <- Conf.newConfig
    pure $ ExpandedStream
        { codes         = cs
        , lexTokens     = []
        , lexState      = Lex.LineBegin
        , expansionMode = Expanding
        , config        = conf
        , skipState     = []
        }

insertControlSequence
    :: ExpandedStream
    -> Lex.ControlSequenceLike
    -> ResolvedToken
    -> GlobalFlag
    -> ExpandedStream
insertControlSequence es@ExpandedStream{config=c} cs t globalFlag =
    es{config=Conf.insertControlSequence cs t globalFlag c}

-- Expanding syntax commands.

expandCSName :: () -> String -> [Lex.Token]
expandCSName _ charToks =
    -- TODO: if control sequence doesn't exist, define one that holds
    -- '\relax'.
    [Lex.ControlSequenceToken $ Lex.ControlSequence charToks]

expandString :: Conf.IntParamVal Conf.EscapeChar -> Lex.Token -> [Lex.Token]
expandString (Conf.IntParamVal escapeCharCode) tok = case tok of
    Lex.CharCatToken _ -> [tok]
    Lex.ControlSequenceToken (Lex.ControlSequence s) ->
        stringAsMadeTokens $ escape ++ s
  where

    escape = if (escapeCharCode < 0) || (escapeCharCode > 255)
        then []
        else [chr escapeCharCode]

expandMacro :: MacroContents -> Map.Map Digit MacroArgument -> [Lex.Token]
expandMacro MacroContents{replacementTokens=(MacroText replaceToks)} args =
    renderMacroText replaceToks
  where
    renderMacroText []     = []
    renderMacroText (t:ts) = render t
      where
        render (MacroTextLexToken x)     = x:rest
        render (MacroTextParamToken dig) = case args !? dig of
            Nothing -> error "No such parameter"
            Just (MacroArgument arg) -> arg ++ rest

        rest = renderMacroText ts

-- Change the case of the parsed tokens.
expandChangeCase :: (VDirection, Conf.Config) -> BalancedText -> [Lex.Token]
expandChangeCase (direction, conf) (BalancedText caseToks) =
    changeCase direction <$> caseToks
  where
    -- Set the character code of each character token to its
    -- \uccode or \lccode value, if that value is non-zero.
    -- Don't change the category code.
    changeCase dir (Lex.CharCatToken (Lex.CharCat char cat)) =
        Lex.CharCatToken $ Lex.CharCat (switch dir char) cat
    changeCase _ t = t

    switch dir char =
        case Conf.lookupChangeCaseCode dir char conf of
            Conf.NoCaseChange       -> char
            Conf.ChangeToCode char' -> char'

skipToIfDelim :: IfBodyState -> ExpandedStream -> Maybe ExpandedStream
skipToIfDelim blk stream = go stream 1
  where
    go :: ExpandedStream -> Int -> Maybe ExpandedStream
    go _stream n =
        do
        (_, rt, _stream') <- fetchResolvedToken _stream
        let cont = go _stream'
        case rt of
            -- If we see an 'if', increment the condition depth.
            SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
                cont $ succ n
            -- If we see an 'end-if'...
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf))
                -- ...and are at top condition depth, we are finished with the
                -- condition block altogether.
                | n == 1 -> Just _stream'
                -- ...otherwise, decrement the condition depth.
                | otherwise -> cont $ pred n
            -- If we see an 'else'...
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                -- ...and are at top condition depth, and our target block is
                -- post-else, we are done skipping tokens. Push a state to prepare
                -- for the later 'end-if'.
                | n == 1, IfPreElse <- blk ->
                    Just _stream'{skipState = (IfBodyState IfPostElse):(skipState _stream')}
                -- (we ignore 'else's even if it is syntactically wrong, if it's
                -- ourside our block of interest.)
            -- Any other token, just skip and continue unchanged.
            _ ->
                cont n


skipUpToCaseBlock ::  Int -> ExpandedStream -> Maybe ExpandedStream
skipUpToCaseBlock tgt stream = go stream 0 1
  where
    go :: ExpandedStream -> Int -> Int -> Maybe ExpandedStream
    go _stream cur n
        -- If we are at top condition depth,
        | n == 1, cur == tgt =
            Just _stream{skipState = (CaseBodyState CasePostOr):(skipState _stream)}
        | otherwise =
            do
            (_, rt, _stream') <- fetchResolvedToken _stream
            let cont = go _stream'
            case rt of
                SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
                    cont cur $ succ n
                SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf))
                    | n == 1 -> Just _stream'
                    | otherwise -> cont cur $ pred n
                SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
                    | n == 1 ->
                        Just _stream'{skipState = (CaseBodyState CasePostElse):(skipState _stream')}
                SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Or))
                    | n == 1 ->
                        cont (succ cur) n
                _ ->
                    cont cur n

runRead
    :: ExpandedStream
    -> ReaderT Conf.Config (Except String) a
    -> Either String a
runRead s f = runExcept $ runReaderT f (config s)

expandConditionToken :: ExpandedStream -> ConditionTok -> (Maybe String, ExpandedStream)
expandConditionToken strm = \case
    ConditionHeadTok ifTok -> case easyRunParser (conditionHeadParser ifTok) strm of
        (P.State resultStream _ _, Left err) ->
            (Just $ show err, resultStream)
        (P.State resultStream _ _, Right a)  ->
            case runRead resultStream (evaluateConditionHead a) of
                Left err ->
                    (Just $ show err, resultStream)
                Right (IfBlockTarget IfPreElse) ->
                    let pushStream = resultStream{skipState = (IfBodyState IfPreElse):(skipState resultStream)}
                    in (Nothing, pushStream)
                Right (IfBlockTarget IfPostElse) ->
                    case skipToIfDelim IfPreElse resultStream of
                        Nothing ->
                            (Just "Ran out of tokens", resultStream)
                        Just skipStream ->
                            (Nothing, skipStream)
                Right (CaseBlockTarget tgt) ->
                    case skipUpToCaseBlock tgt resultStream of
                        Nothing ->
                            (Just "Ran out of tokens", resultStream)
                        Just skipStream ->
                            (Nothing, skipStream)
    ConditionBodyTok delim -> case (delim, skipState strm) of
        -- Shouldn't see any condition token outside a condition block.
        (_, []) ->
            (Just $ "Not in a condition body, but saw condition-body token: " ++ show delim, strm)
        -- Shouldn't see an 'or' while in an if-condition.
        (Or, (IfBodyState _):_) ->
            (Just "In an if-condition, not case-condition, but saw 'or'", strm)
        -- If we see an 'end-if' while in any condition, then pop the
        -- condition.
        (EndIf, _:condRest) ->
            (Nothing, strm{skipState = condRest})
        -- Should not see an 'or' or an 'else' while in a case-condition, while
        -- processing a block started by 'else'. The same goes for seeing an
        -- 'else' while in an if-condition, having already seen an 'else'.
        (Else, (IfBodyState IfPostElse):_) ->
            (Just "Already saw else in this condition-block", strm)
        (Or, (CaseBodyState CasePostElse):_) ->
            (Just "Already saw else in this case-condition, but saw later 'or'", strm)
        (Else, (CaseBodyState CasePostElse):_) ->
            (Just "Already saw else in this condition-block", strm)
        -- If we see a block delimiter while not-skipping, then we must have
        -- been not-skipping the previous block, so we should skip all
        -- remaining blocks.
        -- This applies if we see,
        --     - an 'or' or an 'else' while in a case-condition, while
        --       processing a block started by 'or'
        --     - an 'else' while in an if-condition, before having seen an 'else'
        (Else, (IfBodyState IfPreElse):condRest) ->
            skipToEndOfCondition condRest
        (Or, (CaseBodyState CasePostOr):condRest) ->
            skipToEndOfCondition condRest
        (Else, (CaseBodyState CasePostOr):condRest) ->
            skipToEndOfCondition condRest
  where
    skipToEndOfCondition condRest = case skipToIfDelim IfPostElse strm of
        Nothing ->
            (Just "Ran out of tokens", strm)
        Just skipStream ->
            (Nothing, skipStream{skipState = condRest})

expandSyntaxCommand
    :: ExpandedStream
    -> SyntaxCommandHeadToken
    -> (Either String [Lex.Token], ExpandedStream)
expandSyntaxCommand strm = \case
    MacroTok m ->
        runExpandCommand strm m (parseMacroArgs m) expandMacro
    ConditionTok ct ->
        (\(v, s) -> (justToEither v, s)) $ expandConditionToken strm ct
    NumberTok ->
        undefined
    RomanNumeralTok ->
        undefined
    StringTok ->
        let escapeChar = (Conf.IntParamVal . Conf.lookupIntegerParameter EscapeChar . config) strm
        in runExpandCommand strm escapeChar parseToken expandString
    JobNameTok ->
        undefined
    FontNameTok ->
        undefined
    MeaningTok ->
        undefined
    CSNameTok ->
        runExpandCommand strm () parseCSNameArgs expandCSName
    ExpandAfterTok ->
        case fetchLexToken strm of
            Nothing -> (Left "Ran out of tokens", strm)
            Just (expandAfterArgLT, strm') ->
                case fetchAndExpandToken strm' of
                    Nothing ->
                        (Left "Ran out of tokens", strm')
                    Just (Left err, expandedStrm) ->
                        (Left err, expandedStrm)
                    Just (Right expandedLTs, expandedStrm) ->
                        (Right $ expandAfterArgLT:expandedLTs, expandedStrm)
    NoExpandTok ->
        undefined
    MarkRegisterTok _ ->
        undefined
    InputTok ->
        undefined
    EndInputTok ->
        undefined
    TheTok -> case easyRunParser parseInternalQuantity strm of
        (P.State resultStream _ _, Left err) ->
            (Left $ show err, resultStream)
        (P.State resultStream _ _, Right q)  ->
            case runRead resultStream (showInternalQuantity q) of
                Left err ->
                    (Left $ show err, resultStream)
                Right showS ->
                    (Right $ stringAsMadeTokens showS, resultStream)
    ChangeCaseTok direction ->
        runExpandCommand strm (direction, config strm) parseGeneralText expandChangeCase
  where
    runExpandCommand inputStream com parser f =
        case easyRunParser parser inputStream of
            (P.State resultStream _ _, Left err) ->
                (Left $ show err, resultStream)
            (P.State resultStream _ _, Right a)  ->
                (Right $ f com a, resultStream)

    justToEither Nothing = Right mempty
    justToEither (Just e) = Left e

fetchLexToken :: ExpandedStream -> Maybe (Lex.Token, ExpandedStream)
fetchLexToken stream =
    -- Get the next lex token, and update our stream.
    case lexTokens stream of
        -- If there is a lex token in the buffer, use that.
        (lt:lts) ->
            pure (lt, stream{lexTokens = lts})
        -- If the lex token buffer is empty, extract a token and use it.
        [] ->
            do
            let lkpCatCode t = Conf.lookupCatCode t $ config stream
            (lt, lexState', codes') <-
                Lex.extractToken lkpCatCode (lexState stream) (codes stream)
            pure (lt, stream{codes = codes', lexState = lexState'})

fetchResolvedToken :: ExpandedStream -> Maybe (Lex.Token, ResolvedToken, ExpandedStream)
fetchResolvedToken stream =
    do
    (lt, stream') <- fetchLexToken stream
    -- Resolve the lex token, and inspect the result.
    let lkp cs = Conf.lookupCS cs $ config stream'
    pure (lt, resolveToken lkp (expansionMode stream') lt, stream')

fetchAndExpandToken :: ExpandedStream -> Maybe (Either String [Lex.Token], ExpandedStream)
fetchAndExpandToken ExpandedStream{codes = []} = Nothing
fetchAndExpandToken stream =
    do
    (lt, rt, stream') <- fetchResolvedToken stream
    case rt of
        PrimitiveToken _ -> pure (Right [lt], stream')
        SyntaxCommandHeadToken c ->
            case expandSyntaxCommand stream' c of
                (Left err, expandedStrm) ->
                    pure (Left $ show err, expandedStrm)
                (Right lts, expandedStrm) ->
                    pure (Right lts, expandedStrm)

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
    -- If we've no input, signal that we are done.
    take1_ ExpandedStream{codes = []} = Nothing
    take1_ stream =
        do
        (_, rt, stream') <- fetchResolvedToken stream
        case rt of
            -- If it's a primitive token, provide that.
            PrimitiveToken pt -> pure (pt, stream')
            -- If it indicates the start of a syntax command, parse the
            -- remainder of the syntax command.
            SyntaxCommandHeadToken c ->
                let (errOrLTs, expandStream) = expandSyntaxCommand stream' c
                in case errOrLTs of
                    Left err ->
                        pure (SubParserError err, expandStream)
                    Right lts ->
                        P.take1_ $ insertLexTokens expandStream lts

    takeN_ = undefined

    takeWhile_ = undefined

    showTokens Proxy = show

    reachOffset _ _freshState = (freshSourcePos, "", _freshState)

instance InhibitableStream ExpandedStream where
    setExpansion mode = P.updateParserState $ setStateExpansion
      where
        setStateExpansion est@P.State{stateInput=es} =
          est{P.stateInput = es{expansionMode = mode}}

    getConfig = config

    insertLexToken s t = s{lexTokens = t : lexTokens s}

instance Eq ExpandedStream where
    _ == _ = True

instance Ord (ParseErrorBundle ExpandedStream) where
    compare _ _ = EQ

instance P.ShowErrorComponent (ParseErrorBundle ExpandedStream) where
    showErrorComponent (P.ParseErrorBundle errs _) =
        Fold.concat $ NE.intersperse "\n\n" $ P.showErrorComponent <$> errs

instance Ord (ParseError ExpandedStream) where
    compare _ _ = EQ

instance P.ShowErrorComponent (ParseError ExpandedStream) where
    showErrorComponent (P.TrivialError offset (Just (P.Tokens unexpecteds)) expecteds) =
        "Error at " ++ show offset ++ ".\n"
        ++ "Found unexpected tokens: " ++ show (NE.toList unexpecteds) ++ ".\n"
        ++ "Expected one of: " ++ show (Set.toList expecteds)
    showErrorComponent (P.TrivialError offset Nothing expecteds) =
        "Error at " ++ show offset ++ ".\n"
        ++ "Found no unexpected tokens.\n"
        ++ "Expected one of: " ++ show (Set.toList expecteds)
    showErrorComponent (P.TrivialError offset (Just P.EndOfInput) expecteds) =
        "Error at " ++ show offset ++ ".\n"
        ++ "Found end of input.\n"
        ++ "Expected one of: " ++ show (Set.toList expecteds)
    showErrorComponent (P.TrivialError offset (Just (P.Label lab)) expecteds) =
        "Error at " ++ show offset ++ ".\n"
        ++ "Found label: " ++ show lab ++ ".\n"
        ++ "Expected one of: " ++ show (Set.toList expecteds)
    showErrorComponent (P.FancyError offset sth) =
        "Error at " ++ show offset ++ ".\n"
        ++ "Found fancy error: " ++ show sth ++ ".\n"
