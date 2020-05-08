{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.Stream.Expanding where

import qualified Optics as O
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Generics.Product.Typed as G.P
import qualified Data.List.NonEmpty as L.NE
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Path
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Parse.AST
import Hex.Parse.Assignment
import Hex.Parse.Command
import Hex.Parse.Condition
import Hex.Parse.Stream.Class
import Hex.Parse.SyntaxCommand
import qualified Hex.Quantity as Q
import Hex.Resolve
import Hexlude
import Path (Abs, File, Path)
import qualified Path
import qualified Path.IO

data ExpandingStream
  = ExpandingStream
      { streamTokenSources :: L.NE.NonEmpty TokenSource
      , lexState :: Lex.LexState
      , resolutionMode :: ResolutionMode
      , skipState :: [ConditionBodyState]
      }
  deriving stock (Generic)

newExpandStream :: Maybe (Path Abs File) -> BS.L.ByteString -> ExpandingStream
newExpandStream maybePath cs =
  ExpandingStream
    { streamTokenSources = pure (newTokenSource maybePath cs)
    , lexState = Lex.LineBegin
    , resolutionMode = Resolving
    , skipState = []
    }

instance Describe ExpandingStream where
    describe ExpandingStream { lexState, resolutionMode, skipState, streamTokenSources } =
      [ (0, "ExpandingStream")
      ,   (1, "lexState " <> quote (show lexState))
      ,   (1, "skipState " <> quote (show skipState))
      ,   (1, "resolutionMode " <> quote (show resolutionMode))
      ]
      <> describeNamedRelFoldable1 "tokenSources" streamTokenSources

-- instance
--   ( MonadError e m
--   , AsTeXParseErrors e
--   , AsType ExpansionError e
--   , AsType Data.Path.PathError e
--   , MonadIO m
--   , MonadState st m -- Read-only
--   , HasType Conf.Config st
--   )
--   => P.Stream ExpandingStream m where

--   type Token ExpandingStream = PrimitiveToken

--   type Tokens ExpandingStream = Seq PrimitiveToken

--   -- take1_ :: s -> m (Maybe (Token s, s))
--   take1_ stream =
--     withJust (fetchAndExpandToken stream) $ \(lts, rt, newStream) -> case newStream `seq` rt of
--       -- If it's a primitive token, provide that.
--       PrimitiveToken pt ->
--         pure $ Just (pt, newStream)
--       -- If it indicates the start of a syntax command, parse the
--       -- remainder of the syntax command.
--       SyntaxCommandHeadToken _ ->
--         P.take1_ $ insertLexTokens newStream lts

--   -- tokensToChunk :: Proxy s -> Proxy m -> [Token s] -> Tokens s
--   tokensToChunk _ _ = Seq.fromList

--   -- chunkToTokens :: Proxy s -> Proxy m -> Tokens s -> [Token s]
--   chunkToTokens _ _ = toList

--   -- chunkLength :: Proxy s -> Proxy m -> Tokens s -> Int
--   chunkLength _ _ = length

--   -- If n <= 0, return 'Just (mempty, s)', where s is the original stream.
--   -- If n > 0 and the stream is empty, return Nothing.
--   -- Otherwise, take a chunk of length n, or shorter if the stream is
--   -- not long enough, and return the chunk along with the rest of the stream.
--   -- takeN_ :: Int -> s -> m (Maybe (Tokens s, s))
--   takeN_ = go mempty
--     where
--       go acc n strm
--         | n <= 0 = pure $ Just (acc, strm)
--         | otherwise =
--           P.take1_ strm >>= \case
--             Nothing ->
--               pure $ case acc of
--                 Empty -> Nothing
--                 _ -> Just (acc, strm)
--             Just (t, newS) ->
--               go (acc |> t) (pred n) newS

--   -- Extract chunk while the supplied predicate returns True.
--   -- takeWhile_ :: (Token s -> Bool) -> s -> m (Tokens s, s)
--   takeWhile_ f = go mempty
--     where
--       go acc s =
--         P.take1_ s >>= \case
--           Just (t, newS)
--             | f t ->
--               go (acc |> t) newS
--           _ ->
--             pure (acc, s)

fetchPrimitiveToken
  :: ( MonadError e m
     , AsTeXParseErrors e
     , AsType ExpansionError e
     , AsType Data.Path.PathError e
     , AsType ParseError e

     , MonadIO m

     , MonadState st m
     , HasType Conf.Config st
     )
  => ExpandingStream
  -> m (ExpandingStream, Either ParseError PrimitiveToken)
fetchPrimitiveToken s =
  fetchAndExpandToken s >>= \case
      Nothing ->
        pure (s, Left $ ParseError "End of input")
      Just (lts, rt, s') ->
        -- Tamp down dat memory.
        case s' `seq` rt of
          -- If it's a primitive token, provide that.
          PrimitiveToken pt ->
            pure (s', Right pt)
          -- If it indicates the start of a syntax command, parse the
          -- remainder of the syntax command.
          SyntaxCommandHeadToken _ ->
            fetchPrimitiveToken (insertLexTokens s' lts)

fetchPrimitiveTokenT
  :: ( MonadError e m
     , AsTeXParseErrors e
     , AsType ExpansionError e
     , AsType Data.Path.PathError e
     , AsType ParseError e

     , MonadIO m

     , MonadState st m
     , HasType Conf.Config st
     )
  => TeXParseT ExpandingStream m PrimitiveToken
fetchPrimitiveTokenT = TeXParseT fetchPrimitiveToken

instance
  ( MonadError e m
  , AsTeXParseErrors e
  , AsType ExpansionError e
  , AsType Data.Path.PathError e
  , AsType ParseError e -- TODO: Remove

  , MonadIO m

  , MonadState st m -- Read-only
  , HasType Conf.Config st
  )
  => MonadTeXParse (TeXParseT ExpandingStream m) where

  parseError e = TeXParseT $ \s -> pure (s, Left e)

  -- satisfyThen :: (PrimitiveToken -> Maybe a) -> m a
  satisfyThen f = do
    pt <- fetchPrimitiveTokenT
    case f pt of
      Nothing ->
        -- throwError $ injectTyped $ ParseError $ "Saw unexpected primitive token: " <> show pt
        empty
      Just a ->
        pure a

--     withJust (fetchAndExpandToken stream) $ \(lts, rt, newStream) -> case newStream `seq` rt of
--       -- If it's a primitive token, provide that.
--       PrimitiveToken pt ->
--         pure $ Just (pt, newStream)
--       -- If it indicates the start of a syntax command, parse the
--       -- remainder of the syntax command.
--       SyntaxCommandHeadToken _ ->
--         P.take1_ $ insertLexTokens newStream lts

  -- withInhibition :: m a -> m a
  withInhibition (TeXParseT parse) = TeXParseT $ \s -> do
    let sInhibited = inhibitResolution s
    (s', errOrA) <- parse sInhibited
    pure (enableResolution s', errOrA)

  -- takeWhileP :: (PrimitiveToken -> Bool) -> m (Seq PrimitiveToken)
  takeWhileP f = TeXParseT (go mempty)
    where
      go acc s = do
        (s', errOrPT) <- fetchPrimitiveToken s
        case errOrPT of
          Right pt | f pt ->
            go (acc |> pt) s'
          _ ->
            pure (s, Right acc)

  -- takeResolvedToken :: m ResolvedToken
  takeResolvedToken = TeXParseT $ \s ->
    fetchResolvedToken s <&> \case
      Nothing ->
        (s, Left $ ParseError "End of input")
      Just (_, rt, s') ->
        (s', Right rt)

  -- -- pushSkipState :: ConditionBodyState -> m ()
  pushSkipState cbs = TeXParseT $ \s ->
    pure (s & conditionBodyStateLens %~ O.cons cbs, Right ())

  -- peekSkipState :: m (Maybe ConditionBodyState)
  peekSkipState = TeXParseT $ \s ->
    pure (s, Right $ s ^. conditionBodyStateLens % to headMay)

  -- -- popSkipState :: m (Maybe ConditionBodyState)
  popSkipState = TeXParseT $ \s ->
    pure $ case s ^. conditionBodyStateLens % to O.uncons of
      Nothing ->
        (s, Right Nothing)
      Just (x, xs) ->
        (s & conditionBodyStateLens .~ xs, Right $ Just x)

instance TeXStream ExpandingStream where

  resolutionModeLens = G.P.typed @ResolutionMode

  tokenSourceLens = G.P.typed @(L.NE.NonEmpty TokenSource)

  lexStateLens = G.P.typed @Lex.LexState

  conditionBodyStateLens = G.P.typed @[ConditionBodyState]

-- Expanding syntax commands.
expandCSName :: Applicative t => Seq Code.CharCode -> t Lex.Token
expandCSName charToks =
  -- TODO: if control sequence doesn't exist, define one that holds
  -- '\relax'.
  pure (Lex.ControlSequenceToken $ Lex.mkControlSequence charToks)

expandString
  :: Conf.IntParamVal 'EscapeChar
  -> Lex.Token
  -> Seq Lex.Token
expandString (Conf.IntParamVal escapeCharCodeInt) tok = case tok of
  Lex.CharCatToken _ ->
    singleton tok
  Lex.ControlSequenceToken Lex.ControlSequence {Lex.csChars} ->
    charCodeAsMadeToken <$> addEscapeChar csChars
  where
    addEscapeChar = case Code.fromTeXInt escapeCharCodeInt of
      Nothing -> id
      Just escapeCharCode ->
        if (escapeCharCodeInt < 0) || (escapeCharCodeInt > 255)
        then id
        else (escapeCharCode :<|)

expandMacro
  :: ( MonadError e m
     , AsType ExpansionError e
     )
  => MacroContents
  -> Map.Map Digit MacroArgument
  -> m (Seq Lex.Token)
expandMacro MacroContents {replacementTokens = (MacroText replaceToks)} args =
  mconcatMapM renderToken replaceToks
  where
    mconcatMapM f = fmap fold . mapM f

    renderToken = \case
      MacroTextLexToken x ->
        pure (singleton x)
      MacroTextParamToken dig -> case args !? dig of
        Nothing ->
          throwError $ injectTyped $ ExpansionError "No such parameter"
        Just (MacroArgument arg) ->
          pure arg

-- Change the case of the parsed tokens.
-- Set the character code of each character token to its
-- \uccode or \lccode value, if that value is non-zero.
-- Don't change the category code.
expandChangeCase
  :: (Code.CharCode -> Conf.CaseChangeCode)
  -> BalancedText
  -> Seq Lex.Token
expandChangeCase lookupChangeCaseCode (BalancedText caseToks) =
  changeCase <$> caseToks
  where
    changeCase = \case
      Lex.CharCatToken (Lex.CharCat char cat) ->
        Lex.CharCatToken $
          Lex.CharCat (switch char) cat
      t -> t
    switch char = case lookupChangeCaseCode char of
      Conf.NoCaseChange -> char
      Conf.ChangeToCode char' -> char'

skipToIfDelim
  :: forall m
   . ( MonadTeXParse m)
  => IfBodyState
  -> m ()
skipToIfDelim blk = go 1
  where
    go :: Int -> m ()
    go n =
      takeResolvedToken >>= \case
        -- If we see an 'if', increment the condition depth.
        SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
          go $ succ n
        -- If we see an 'end-if'...
        -- ...and are at top condition depth, we are finished with the
        -- condition block altogether.
        -- ...otherwise, decrement the condition depth.
        SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf))
          | n == 1 ->
              pure ()
          | otherwise ->
            go $ pred n
        -- If we see an 'else' and are at top condition depth, and our
        -- target block is post-else, we are done skipping tokens. Push a
        -- state to prepare for the later 'end-if'.
        SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
          | n == 1
          , IfPreElse <- blk ->
              pushSkipState (IfBodyState IfPostElse)
        -- (we ignore 'else's even if it is syntactically wrong, if it's
        -- ourside our block of interest.)
        -- Any other token, just skip and continue unchanged.
        _ ->
          go n

skipUpToCaseBlock
  :: forall m
   . MonadTeXParse m
  => Q.TeXInt
  -> m ()
skipUpToCaseBlock tgt = go 0 1
  where
    go :: Q.TeXInt -> Int -> m ()
    go cur n
      | n == 1
      , cur == tgt =
        -- If we are at top condition depth,
        pushSkipState (CaseBodyState CasePostOr)
      | otherwise =
          takeResolvedToken >>= \case
            SyntaxCommandHeadToken (ConditionTok (ConditionHeadTok _)) ->
              go cur $ succ n
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok EndIf))
              | n == 1 ->
                pure ()
              | otherwise ->
                go cur $ pred n
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Else))
              | n == 1 ->
                pushSkipState (CaseBodyState CasePostElse)
            SyntaxCommandHeadToken (ConditionTok (ConditionBodyTok Or))
              | n == 1 ->
                go (succ cur) n
            _ ->
              go cur n

expandConditionToken
  :: forall st e m
   . ( TeXParseCtx st e m
     , AsType ExpansionError e
     )
  => ConditionTok
  -> m ()
expandConditionToken = \case
  ConditionHeadTok ifTok -> do
    condHead <- conditionHeadParser ifTok
    conf <- gets $ getTyped @Conf.Config
    runReaderT (texEvaluate condHead) conf >>= \case
      IfBlockTarget IfPreElse ->
        pushSkipState (IfBodyState IfPreElse)
      IfBlockTarget IfPostElse ->
        skipToIfDelim IfPreElse
      CaseBlockTarget tgt ->
        skipUpToCaseBlock tgt
  ConditionBodyTok delim -> do
    maySkipState <- peekSkipState
    case (delim, maySkipState) of
      -- Shouldn't see any condition token outside a condition block.
      (_, Nothing) ->
        throwError $ injectTyped $ ExpansionError $ "Not in a condition body, but saw condition-body token: " <> Hexlude.show delim
      -- Shouldn't see an 'or' while in an if-condition.
      (Or, Just (IfBodyState _)) ->
        throwError $ injectTyped $ ExpansionError "In an if-condition, not case-condition, but saw 'or'"
      -- If we see an 'end-if' while in any condition, then pop the
      -- condition.
      (EndIf, Just _) ->
        void popSkipState
      -- Should not see an 'or' or an 'else' while in a case-condition, while
      -- processing a block started by 'else'. The same goes for seeing an
      -- 'else' while in an if-condition, having already seen an 'else'.
      (Else, Just (IfBodyState IfPostElse)) ->
        throwError $ injectTyped $ ExpansionError "Already saw 'else' in this condition-block, but saw later 'else'"
      (Or, Just (CaseBodyState CasePostElse)) ->
        throwError $ injectTyped $ ExpansionError "Already saw 'else' in this case-condition, but saw later 'or'"
      (Else, Just (CaseBodyState CasePostElse)) ->
        throwError $ injectTyped $ ExpansionError "Already saw else in this condition-block, but saw later 'else'"
      -- If we see a block delimiter while not-skipping, then we must have
      -- been not-skipping the previous block, so we should skip all
      -- remaining blocks.
      -- This applies if we see,
      --     - an 'or' or an 'else' while in a case-condition, while
      --       processing a block started by 'or'
      --     - an 'else' while in an if-condition, before having seen an 'else'
      (Else, Just (IfBodyState IfPreElse)) ->
        skipToEndOfCondition
      (Or, Just (CaseBodyState CasePostOr)) ->
        skipToEndOfCondition
      (Else, Just (CaseBodyState CasePostOr)) ->
        skipToEndOfCondition
  where
    skipToEndOfCondition = do
      skipToIfDelim IfPostElse
      void popSkipState

expandSyntaxCommand
  :: ( MonadIO m

     , MonadState st m -- Read-only
     , HasType Conf.Config st

     , MonadError e m
     , AsTeXParseErrors e
     , AsType ParseError e
     , AsType ExpansionError e
     , AsType Data.Path.PathError e
     )
  => ExpandingStream
  -> SyntaxCommandHeadToken
  -> m (Maybe (ExpandingStream, Seq Lex.Token))
expandSyntaxCommand strm = \case
  MacroTok m ->
    runExpandCommand strm (parseMacroArgs m) (expandMacro m)
  ConditionTok ct -> do
    (newStrm, ()) <- runTeXParseTEmbedded (expandConditionToken ct) strm
    pure $ Just (newStrm, mempty)
  NumberTok ->
    panic "Not implemented: syntax command NumberTok"
  RomanNumeralTok ->
    panic "Not implemented: syntax command RomanNumeralTok"
  StringTok -> do
    conf <- gets $ getTyped @Conf.Config
    let escapeChar = (Conf.IntParamVal . Conf.lookupTeXIntParameter EscapeChar) conf
    (postArgStream, lexTok) <- runTeXParseTEmbedded parseLexToken strm
    pure $ Just (postArgStream, expandString escapeChar lexTok)
  JobNameTok ->
    panic "Not implemented: syntax command JobNameTok"
  FontNameTok ->
    panic "Not implemented: syntax command FontNameTok"
  MeaningTok ->
    panic "Not implemented: syntax command MeaningTok"
  CSNameTok ->
    runExpandCommand strm parseCSNameArgs (expandCSName >>> pure)
  ExpandAfterTok -> fetchLexToken strm >>= \case
    Nothing -> pure Nothing
    Just (argLT, postArgStream) ->
      withJust (fetchAndExpandToken postArgStream) $ \(postArgLTs, _, postExpandStream) ->
        -- Prepend the unexpanded token.
        pure $ Just (postExpandStream, argLT :<| postArgLTs)
  NoExpandTok ->
    panic "Not implemented: syntax command NoExpandTok"
  MarkRegisterTok _ ->
    panic "Not implemented: syntax command MarkRegisterTok"
  -- \input ⟨file name⟩:
  -- - Expand to no tokens
  -- - Prepare to read from the specified file before looking at any more
  --   tokens from the current source.
  InputTok -> do
    (resultStream@ExpandingStream {streamTokenSources}, TeXFilePath texPath) <- runTeXParseTEmbedded parseFileName strm
    let extraDirs = case sourcePath $ L.NE.head streamTokenSources of
          Just p -> [Path.parent p]
          Nothing -> []
    absPath <- Path.IO.makeAbsolute texPath
    conf <- gets $ getTyped @Conf.Config
    path <- runReaderT (Conf.findFilePath (Conf.WithImplicitExtension "tex") extraDirs texPath) conf
    newSource <- newTokenSource (Just absPath) <$> Code.readCharCodes path
    let newResultStream = resultStream & G.P.typed @(L.NE.NonEmpty TokenSource) %~ L.NE.cons newSource
    pure $ Just (newResultStream, mempty)
  EndInputTok ->
    panic "Not implemented: syntax command EndInputTok"
  TheTok -> do
    (resultStream, intQuant) <- runTeXParseTEmbedded parseInternalQuantity strm
    quantTokens <- fmap charCodeAsMadeToken <$> texEvaluate intQuant
    pure $ Just (resultStream, quantTokens)
  ChangeCaseTok direction -> do
    conf <- gets $ getTyped @Conf.Config
    runExpandCommand strm parseGeneralText $ expandChangeCase (\c -> Conf.lookupChangeCaseCode direction c conf) >>> pure
  where
    runExpandCommand inputStream parser f = do
      (stream, a) <- runTeXParseTEmbedded parser inputStream
      v <- f a
      pure $ Just (stream, v)

fetchAndExpandToken
  :: ( MonadIO m

     , MonadState st m
     , HasType Conf.Config st

     , MonadError e m
     , AsTeXParseErrors e
     , AsType ParseError e
     , AsType ExpansionError e
     , AsType Data.Path.PathError e
     )
  => ExpandingStream
  -> m (Maybe (Seq Lex.Token, ResolvedToken, ExpandingStream))
fetchAndExpandToken stream =
  withJust (fetchResolvedToken stream) $ \(lt, rt, newStream) -> case rt of
    PrimitiveToken _ ->
      -- putText $ "$> ---- Got Lex-token: " <> describe lt <> "\n*> ---- Resolved to Primitive-token: " <> describe pt <> "\n\n"
      pure $ Just (singleton lt, rt, newStream)
    SyntaxCommandHeadToken c ->
      withJust (expandSyntaxCommand newStream c) $ \(expandStream, lts) ->
        -- putText $ "$> ---- Got Lex-token: " <> describe lt <> "\n*> ---- Resolved to Syntax-command-head-token: " <> describe c <> "\n>> ---- Expanded to lex-tokens: " <> describe lts <> "\n\n"
        pure $ Just (lts, rt, expandStream)
