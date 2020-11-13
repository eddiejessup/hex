{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.Parse where

import qualified Data.ByteString as BS
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Path as Path
import qualified Data.Sequence as Seq
import qualified Hex.Config as Conf
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Parse.AST
import Hex.Parse.CommandParser.Assignment
import Hex.Parse.CommandParser.Condition
import Hex.Parse.CommandParser.Command
import Hex.Parse.CommandParser.Inhibited
import Hex.Parse.Stream.Class
import Hex.Parse.TokenParser.Class
import Hex.Parse.TokenParser.Combinators
import Hex.Parse.TokenParser.ParseT
import Hex.Parse.CommandParser.SyntaxCommand
import qualified Hex.Quantity as Q
import Hex.Resolve
import Hexlude
import Path (Rel, File, Path)
import Hex.Config (ConfigError)
import qualified Optics.Cons as O.Cons

instance
  ( MonadError e (TeXParseT s m)
  , AsType ExpansionError e
  , AsType EvaluationError e
  , AsType Path.PathError e
  , AsType ResolutionError e
  , AsType Lex.LexError e
  , AsType ConfigError e

  , Path.MonadInput m
  -- , MonadSlog m

  , MonadState st m -- Read-only
  , HasType Conf.Config st

  , TeXStream s
  )
  => MonadTokenParse (TeXParseT s m) where

  parseError = pParseError

  satisfyThen :: (PrimitiveToken -> Maybe a) -> TeXParseT s m a
  satisfyThen = pSatisfyThen

  withInhibition :: TeXParseT s m a -> TeXParseT s m a
  withInhibition = pWithInhibition

  takeWhileP :: (PrimitiveToken -> Bool) -> TeXParseT s m (Seq PrimitiveToken)
  takeWhileP f = TeXParseT $ \s -> go mempty s
    where
      go acc s = do
        let (TeXParseT parsePrimToken) = pFetchPrimitiveToken
        (s', errOrPt) <- parsePrimToken s
        case errOrPt of
          Right pt | f pt ->
            go (acc |> pt) s'
          _ ->
            pure (s, Right acc)

  takeLexToken :: TeXParseT s m Lex.Token
  takeLexToken = pTakeLexToken

  takeAndResolveLexToken :: TeXParseT s m (Lex.Token, ResolvedToken)
  takeAndResolveLexToken = pTakeAndResolveLexToken

  takeAndExpandResolvedToken :: TeXParseT s m (Lex.Token, ExpandedToken)
  takeAndExpandResolvedToken = pTakeAndExpandResolvedToken

  pushSkipState :: ConditionBodyState -> TeXParseT s m ()
  pushSkipState cbs = TeXParseT $ \st -> do
    pure (st & field @"skipState" %~ O.Cons.cons cbs, Right ())

  peekSkipState :: TeXParseT s m (Maybe ConditionBodyState)
  peekSkipState = TeXParseT $ \st -> do
    pure (st, Right $ st ^. field @"skipState" % to headMay)

  popSkipState :: TeXParseT s m (Maybe ConditionBodyState)
  popSkipState = TeXParseT $ \st -> do
    pure $ case st ^. field @"skipState" % to O.Cons.uncons of
      Nothing ->
        (st, Right Nothing)
      Just (x, xs) ->
        (st & field @"skipState" .~ xs, Right $ Just x)

  inputPath :: Path Rel File -> TeXParseT s m ()
  inputPath texPath = TeXParseT $ \st -> do
    searchDirs <- use (typed @Conf.Config % field @"searchDirectories")
    absTexPath <- Path.findPath (Path.WithImplicitExtension "tex") texPath searchDirs
    codes <- Path.readPathBytes absTexPath
    let newSource = newTokenSource (Just absTexPath) codes
    pure (st & field @"texStream" %~ flip addTokenSource newSource, Right ())

  insertLexTokens :: Seq Lex.Token -> TeXParseT s m ()
  insertLexTokens = pInsertLexTokens

pParseError :: Applicative m => ParseError -> TeXParseT s m a
pParseError e = TeXParseT $ \s -> pure (s, Left e)


pSatisfyThen
  :: ( MonadError e (TeXParseT s m)
     , AsType Lex.LexError e
     , AsType ExpansionError e
     , AsType EvaluationError e
     , AsType ResolutionError e
     , AsType ConfigError e
     , AsType Path.PathError e

     , Path.MonadInput (TeXParseT s m)

     , MonadState st m
     , HasType Conf.Config st

     , TeXStream s
     ) => (PrimitiveToken -> Maybe a) -> TeXParseT s m a
pSatisfyThen f = try $ do
    pt <- pFetchPrimitiveToken
    case f pt of
      Nothing ->
        pParseError $ ParseErrorWithMsg $ "Saw unexpected primitive token: " <> show pt
      Just a ->
        pure a

try :: Functor m => TeXParseT s m a -> TeXParseT s m a
try (TeXParseT parse) = TeXParseT $ \s ->
  parse s <&> \case
    -- If parse fails, revert to the old stream.
    (_, Left err) -> (s, Left err)
    -- If parse succeeds, stick with the new stream.
    (s', Right a) -> (s', Right a)

pFetchPrimitiveToken
  :: ( MonadError e m
     , AsType ExpansionError e
     , AsType EvaluationError e
     , AsType ResolutionError e
     , AsType ConfigError e
     , AsType Path.PathError e
     , AsType Lex.LexError e

    --  , MonadSlog m

     , TeXStream s

     , Path.MonadInput m

     , MonadState st m
     , HasType Conf.Config st
     )
  => TeXParseT s m PrimitiveToken
pFetchPrimitiveToken = do
  (_lt, et) <- pTakeAndExpandResolvedToken
  case et of
    ExpandedPrimitiveToken pt ->
      pure pt
    -- If we just saw a syntax command, insert the resulting expanded lex-tokens
    -- into the input, and try again.
    ExpandedSyntaxCommand _c lts -> do
      -- lift $ sLogStampedJSON "Inserting lex tokens and reading again"
      --   [ ("lexTokens", toJSON lts)
      --   , ("SyntaxCommandHeadToken", toJSON c)
      --   ]
      pInsertLexTokens lts
      pFetchPrimitiveToken

pInsertLexTokens :: (Applicative m, TeXStream s) => Seq Lex.Token -> TeXParseT s m ()
pInsertLexTokens lts = TeXParseT $ \st -> do
    pure (st & field @"texStream" %~ flip insertLexTokensToStream lts, Right ())

pTakeLexToken :: forall e s m. (MonadError e m, AsType Lex.LexError e, TeXStream s) => TeXParseT s m Lex.Token
pTakeLexToken = TeXParseT takeLexTokenS
  where
    takeLexTokenS :: TeXParseState s -> m (TeXParseState s, Either ParseError Lex.Token)
    takeLexTokenS st = do
      extractLexToken (st ^. field @"texStream") <&> \case
        Nothing ->
          (st, Left EndOfInput)
        Just (lt, s') ->
          (st & field @"texStream" .~ s', Right lt)

pTakeAndResolveLexToken
  :: forall e s st m.
     ( MonadError e m
     , AsType ResolutionError e
     , AsType Lex.LexError e

     , MonadState st m
     , HasType Conf.Config st

     , TeXStream s
     )
  => TeXParseT s m (Lex.Token, ResolvedToken)
pTakeAndResolveLexToken = do
  lt <- pTakeLexToken
  TeXParseT $ resolveLexTokenS lt
  where
    resolveLexTokenS :: Lex.Token -> TeXParseState s -> m (TeXParseState s, Either ParseError (Lex.Token, ResolvedToken))
    resolveLexTokenS lt st = do
      conf <- use (typed @Conf.Config)
      let csLkp cs = Conf.lookupCS cs conf
      let mayRT = resolveToken csLkp (st ^. field @"resolutionMode") lt
      rt <-
        note
          (injectTyped $ ResolutionError $ "Could not resolve lex token: " <> renderDescribed lt)
          mayRT
      pure (st, Right (lt, rt))


pTakeAndExpandResolvedToken
  :: ( MonadState st m
     , HasType Conf.Config st

     , TeXStream s

     , MonadError e m
     , AsType Lex.LexError e
     , AsType ExpansionError e
     , AsType EvaluationError e
     , AsType ConfigError e
     , AsType ResolutionError e
     , AsType Path.PathError e

     , Path.MonadInput m

    --  , MonadSlog m
     )
  => TeXParseT s m (Lex.Token, ExpandedToken)
pTakeAndExpandResolvedToken = do
  (lt, rt) <- pTakeAndResolveLexToken
  et <- case rt of
    -- If it's a primitive token, provide that.
    PrimitiveToken pt ->
      pure $ ExpandedPrimitiveToken pt
    -- If it indicates the start of a syntax command, parse the
    -- remainder of the syntax command.
    SyntaxCommandHeadToken c -> do
      lts <- expandSyntaxCommand c
      pure $ ExpandedSyntaxCommand c lts
  pure (lt, et)

-- -- Expanding syntax commands.
expandCSName :: Applicative t => [Code.CharCode] -> t Lex.Token
expandCSName charToks =
  -- TODO: if control sequence doesn't exist, define one that holds
  -- '\relax'.
  pure (Lex.ControlSequenceToken $ Lex.mkControlSequence charToks)

expandString
  :: Conf.IntParamVal 'EscapeChar
  -> Lex.Token
  -> Seq Lex.Token
expandString (Conf.IntParamVal escapeCharCodeInt) tok =
  case tok of
    Lex.CharCatToken _ ->
      singleton tok
    Lex.ControlSequenceToken (Lex.ControlSequence bs) ->
      let csCodes = Seq.fromList $ Code.CharCode <$> BS.unpack bs
      in charCodeAsMadeToken <$> case Code.fromTeXInt escapeCharCodeInt of
        Nothing -> csCodes
        Just escapeCharCode
          | (escapeCharCodeInt < 0) || (escapeCharCodeInt > 255) ->
            csCodes
          | otherwise ->
            escapeCharCode <| csCodes

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

skipToIfDelim :: forall m. MonadTokenParse m => IfBodyState -> m ()
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
   . MonadTokenParse m
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
  :: ( MonadTokenParse m

     , MonadEvaluate m ConditionHead

     , MonadError e m
     , AsType ExpansionError e
     )
  => ConditionTok
  -> m ()
expandConditionToken = \case
  ConditionHeadTok ifTok -> do
    condHead <- conditionHeadParser ifTok
    astEval condHead >>= \case
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
  :: ( MonadState st m -- Read-only
     , HasType Conf.Config st

     , MonadTokenParse m

     , MonadEvaluate m ConditionHead
     , MonadEvaluate m InternalQuantity

     , MonadError e m
     , AsType ExpansionError e
     )
  => SyntaxCommandHeadToken
  -> m (Seq Lex.Token)
expandSyntaxCommand = \case
  MacroTok m -> do
    args <- parseMacroArgs m
    expandMacro m args
  ConditionTok ct -> do
    expandConditionToken ct
    pure mempty
  NumberTok ->
    panic "Not implemented: syntax command NumberTok"
  RomanNumeralTok ->
    panic "Not implemented: syntax command RomanNumeralTok"
  StringTok -> do
    conf <- use $ typed @Conf.Config
    let escapeChar = (Conf.IntParamVal . Conf.lookupTeXIntParameter EscapeChar) conf
    expandString escapeChar <$> parseLexToken
  JobNameTok ->
    panic "Not implemented: syntax command JobNameTok"
  FontNameTok ->
    panic "Not implemented: syntax command FontNameTok"
  MeaningTok ->
    panic "Not implemented: syntax command MeaningTok"
  CSNameTok -> do
    a <- parseCSNameArgs
    singleton <$> expandCSName a
  ExpandAfterTok -> do
    argLT <- takeLexToken
    (lt, et) <- takeAndExpandResolvedToken
    let
      expandedLts = case et of
        -- If the after-token is a primitive token, its expansion is just its
        -- corresponding pre-resolution lex token.
        ExpandedPrimitiveToken _ -> singleton lt
        ExpandedSyntaxCommand _c lts -> lts
    -- Prepend the unexpanded token.
    pure (argLT <| expandedLts)
  NoExpandTok ->
    panic "Not implemented: syntax command NoExpandTok"
  MarkRegisterTok _ ->
    panic "Not implemented: syntax command MarkRegisterTok"
  -- \input ⟨file name⟩:
  -- - Expand to no tokens
  -- - Prepare to read from the specified file before looking at any more
  --   tokens from the current source.
  InputTok -> do
    TeXFilePath texPath <- parseFileName
    inputPath texPath
    pure mempty
  EndInputTok ->
    panic "Not implemented: syntax command EndInputTok"
  TheTok -> do
    intQuant <- parseInternalQuantity
    fmap charCodeAsMadeToken <$> astEval intQuant
  ChangeCaseTok direction -> do
    conf <- use $ typed @Conf.Config
    expandChangeCase
      (\c -> Conf.lookupChangeCaseCode direction c conf)
      <$> parseGeneralText
