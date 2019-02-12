{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Stream where

import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT
                                                , gets
                                                , modify
                                                , runStateT
                                                )
import           Control.Monad.Reader           ( runReaderT
                                                )
import           Data.Char                      ( toLower
                                                , toUpper
                                                )
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
import           HeX.Parse.Condition
import           HeX.Parse.Inhibited
import           HeX.Parse.SyntaxCommand
import           HeX.Parse.Resolve

skipping :: ExpandedStream -> Bool
skipping (ExpandedStream{..}) = case skipState of
    [] -> False
    blockState:_ -> shouldSkip blockState
  where
    shouldSkip = \case
        ConditionBlockState cur (IfBlockTarget tgt) -> cur /= tgt
        ConditionBlockState PreElse (CaseBlockTarget cur tgt) -> cur /= tgt
        ConditionBlockState PostElse (CaseBlockTarget cur tgt) -> cur > tgt

data ExpandedStream = ExpandedStream
    { codes         :: [Cat.CharCode]
    , lexTokens     :: [Lex.Token]
    , lexState      :: Lex.LexState
    , expansionMode :: ExpansionMode
    , config        :: Conf.Config
    , skipState     :: [ConditionBlockState]
    } deriving (Show)

runConfState :: MonadState ExpandedStream m => StateT Conf.Config m a -> m a
runConfState f =
    do
    conf <- gets config
    (v, conf') <- runStateT f conf
    modify (\stream -> stream{config=conf'})
    pure v

newExpandStream :: [Cat.CharCode] -> CSMap -> IO ExpandedStream
newExpandStream cs _csMap =
    do
    conf <- Conf.newConfig _csMap
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
    -> ExpandedStream
insertControlSequence es@ExpandedStream{config=c} cs t =
    es{config=Conf.insertControlSequence c cs t}

-- Expanding syntax commands.

expandCSName :: () -> String -> [Lex.Token]
expandCSName _ charToks =
    -- TODO: if control sequence doesn't exist, define one that holds
    -- '\relax'.
    [Lex.ControlSequenceToken $ Lex.ControlSequence charToks]

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
expandChangeCase :: VDirection -> BalancedText -> [Lex.Token]
expandChangeCase direction (BalancedText caseToks) =
    changeCase direction <$> caseToks
  where
    -- Set the character code of each character token to its
    -- \uccode or \lccode value, if that value is non-zero.
    -- Don't change the category code.
    changeCase dir (Lex.CharCatToken (Lex.CharCat char cat)) =
        Lex.CharCatToken $ Lex.CharCat (switch dir char) cat
    changeCase _ t = t

    switch Upward   = toUpper
    switch Downward = toLower

runSyntaxCommand
  :: InhibitableStream s
  => s -- The input stream
  -> a -- Any information from the command head.
  -> SimpParser s b -- A parser for the command's arguments, if any.
  -> (s -> a -> b -> s) -- Modify the stream with the above information.
  -> Maybe (PrimitiveToken, s)
runSyntaxCommand inputStream com parser f =
    case easyRunParser parser inputStream of
        (P.State resultStream _ _, Left err) ->
            pure (SubParserError $ show err, resultStream)
        (P.State resultStream _ _, Right a)  ->
            P.take1_ $ f resultStream com a

runExpandCommand
  :: InhibitableStream s
  => s -- The input stream
  -> a -- Any information from the command head.
  -> SimpParser s b -- A parser for the command's arguments, if any.
  -> (a -> b -> [Lex.Token]) -- Combine the above information into the expansion result.
  -> Maybe (PrimitiveToken, s)
runExpandCommand inputStream com parser f =
    runSyntaxCommand inputStream com parser insrt
  where
    insrt strm com_ args = insertLexTokens strm $ f com_ args

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
        -- Get the next lex token, and update our stream.
        (lt, stream') <- case lexTokens stream of
            -- If there is a lex token in the buffer, use that.
            (lt:lts) ->
                pure (lt, stream{lexTokens = lts})
            -- If the lex token buffer is empty, extract a token and use it.
            [] ->
                do
                (lt, lexState', codes') <-
                    Lex.extractToken (Cat.catLookup $ Conf.catCodeMap $ config stream) (lexState stream) (codes stream)
                pure (lt, stream{codes = codes', lexState = lexState'})
        -- Resolve the lex token, and inspect the result.
        case resolveToken (Conf.csMap $ config stream') (expansionMode stream') lt of
            ConditionBlockToken ct -> case (ct, skipState stream') of
                -- Shouldn't see any condition token outside a condition block.
                (_, []) ->
                    pure (SubParserError "not in condition-block", stream')
                -- Shouldn't see an 'or' in an if-block.
                (OrTok, (ConditionBlockState _ (IfBlockTarget _)):_) ->
                    pure (SubParserError "in an if-block, not case-block, but saw 'or'", stream')
                -- If we see an 'or' in the proper context, set the current
                -- block to the next block number and continue.
                (OrTok, (ConditionBlockState PreElse (CaseBlockTarget cur tgt)):condRest) ->
                    P.take1_ stream'{skipState = (ConditionBlockState PreElse (CaseBlockTarget (succ cur) tgt)):condRest}
                (OrTok, (ConditionBlockState PostElse (CaseBlockTarget _ _)):_) ->
                    pure (SubParserError "should not see 'or' in case-block after seeing  'else'", stream')
                -- If we see an 'else' in the proper context, set the current
                -- block to 'saw else' and continue.
                (ElseTok, (ConditionBlockState PreElse v):condRest) ->
                    P.take1_ stream'{skipState = (ConditionBlockState PostElse v):condRest}
                -- Shouldn't see an else after we already saw an else.
                (ElseTok, (ConditionBlockState PostElse _):_) ->
                    pure (SubParserError "already saw else in this condition-block", stream')
                -- If we see an end-if ('\fi') while in a condition block,
                -- pop the block and continue.
                (EndIfTok, _:condRest) ->
                    P.take1_ stream'{skipState = condRest}
            -- If we see a non-condition-token while skipping, just continue
            -- unchanged.
            NonConditionBlockToken _ | skipping stream' -> P.take1_ stream'
            NonConditionBlockToken nt -> case nt of
                -- If it's a primitive token, provide that.
                PrimitiveToken pt -> pure (pt, stream')
                -- If it indicates the start of a syntax command, parse the
                -- remainder of the syntax command.
                SyntaxCommandHeadToken c -> case c of
                    ChangeCaseTok direction ->
                        runExpandCommand stream' direction parseGeneralText expandChangeCase
                    MacroTok m ->
                        runExpandCommand stream' m (parseMacroArgs m) expandMacro
                    CSNameTok ->
                        runExpandCommand stream' () parseCSNameArgs expandCSName
                    IfTok ifTok -> case easyRunParser (conditionHeadParser ifTok) stream' of
                        (P.State resultStream _ _, Left err) ->
                            pure (SubParserError $ show err, resultStream)
                        (P.State resultStream _ _, Right a)  ->
                            do
                            mayBlockTarget <- runExceptT $ runReaderT (evaluateConditionHead a) (config resultStream)
                            case mayBlockTarget of
                                Left err ->
                                    pure (SubParserError $ show err, resultStream)
                                Right blockTarget -> 
                                    P.take1_ resultStream{skipState = ConditionBlockState PreElse blockTarget:(skipState resultStream)}

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
