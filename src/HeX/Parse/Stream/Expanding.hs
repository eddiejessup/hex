{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.Stream.Expanding where

import qualified Optics.Cons.Core as O.Cons
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.ByteString as BS
import qualified Data.Generics.Product as G.P
import qualified Data.List.NonEmpty as L.NE
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Path
import qualified Data.Sequence as Seq
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

pFetchPrimitiveToken
  :: ( MonadError e m
     , AsTeXParseErrors e
     , AsType ExpansionError e
     , AsType Data.Path.PathError e

     , MonadIO m
     , MonadSlog m

     , MonadState st m
     , HasType Conf.Config st
     )
  => TeXParseT ExpandingStream m PrimitiveToken
pFetchPrimitiveToken = do
  (rt, lts) <- pTakeAndExpandResolvedToken
  case rt of
    -- If it's a primitive token, provide that.
    PrimitiveToken pt ->
      pure pt
    -- If it indicates the start of a syntax command, parse the
    -- remainder of the syntax command.
    SyntaxCommandHeadToken t -> do
      lift $ sLogStampedJSON "Inserting lex tokens and reading again"
        [ ("lexTokens", toJSON lts)
        , ("SyntaxCommandHeadToken", toJSON t)
        ]
      pInsertLexTokens lts
      pFetchPrimitiveToken

try :: Monad m => TeXParseT ExpandingStream m a -> TeXParseT ExpandingStream m a
try (TeXParseT parse) = TeXParseT $ \s -> do
  (s', errOrA) <- parse s
  case errOrA of
    Left err ->
      -- If parse fails, revert to the old stream.
      pure (s, Left err)
    Right a ->
      -- If parse succeeds, stick with the new stream.
      pure (s', Right a)

instance
  ( MonadError e m
  , AsTeXParseErrors e
  , AsType ExpansionError e
  , AsType Data.Path.PathError e

  , MonadIO m
  , MonadSlog m

  , MonadState st m -- Read-only
  , HasType Conf.Config st
  )
  => MonadTeXParse (TeXParseT ExpandingStream m) where

  parseError e = TeXParseT $ \s -> pure (s, Left e)

  -- satisfyThen :: (PrimitiveToken -> Maybe a) -> m a
  satisfyThen f = try $ do
    pt <- pFetchPrimitiveToken
    case f pt of
      Nothing ->
        parseError $ ParseErrorWithMsg $ "Saw unexpected primitive token: " <> show pt
      Just a ->
        pure a

  -- withInhibition :: m a -> m a
  withInhibition (TeXParseT parse) = TeXParseT $ \s -> do
    let sInhibited = inhibitResolution s
    (s', errOrA) <- parse sInhibited
    pure (enableResolution s', errOrA)

  -- takeWhileP :: (PrimitiveToken -> Bool) -> m (Seq PrimitiveToken)
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

  takeLexToken = pTakeLexToken

  takeAndResolveLexToken = pTakeAndResolveLexToken

  takeAndExpandResolvedToken = pTakeAndExpandResolvedToken

  -- -- pushSkipState :: ConditionBodyState -> m ()
  pushSkipState cbs = TeXParseT $ \s ->
    pure (s & conditionBodyStateLens %~ O.Cons.cons cbs, Right ())

  -- peekSkipState :: m (Maybe ConditionBodyState)
  peekSkipState = TeXParseT $ \s ->
    pure (s, Right $ s ^. conditionBodyStateLens % to headMay)

  -- -- popSkipState :: m (Maybe ConditionBodyState)
  popSkipState = TeXParseT $ \s ->
    pure $ case s ^. conditionBodyStateLens % to O.Cons.uncons of
      Nothing ->
        (s, Right Nothing)
      Just (x, xs) ->
        (s & conditionBodyStateLens .~ xs, Right $ Just x)

  inputPath texPath = TeXParseT $ \s -> do
    let extraDirs = case s ^. tokenSourceLens % neHeadL % G.P.field @"sourcePath" of
          Just p -> [Path.parent p]
          Nothing -> []
    absPath <- Path.IO.makeAbsolute texPath
    path <- Conf.findFilePath (Conf.WithImplicitExtension "tex") extraDirs texPath
    newSource <- newTokenSource (Just absPath) <$> Code.readCharCodes path
    pure (s & tokenSourceLens %~ L.NE.cons newSource, Right ())

  insertLexTokens = pInsertLexTokens

pInsertLexTokens :: (Applicative m, TeXStream s) => Seq Lex.Token -> TeXParseT s m ()
pInsertLexTokens lts = TeXParseT $ \s ->
    pure (insertLexTokensToStream s lts, Right ())

pTakeLexToken :: (MonadState st m, HasType Conf.Config st) => TeXParseT ExpandingStream m Lex.Token
pTakeLexToken = TeXParseT takeLexTokenS

takeLexTokenS
  :: ( MonadState st m
     , HasType Conf.Config st
     )
  => ExpandingStream
  -> m (ExpandingStream, Either ParseError Lex.Token)
takeLexTokenS s =
  fetchLexToken s <&> \case
    Nothing ->
      (s, Left EndOfInput)
    Just (lt, s') ->
      (s', Right lt)

pTakeAndResolveLexToken
  :: ( MonadError e m
     , AsType ResolutionError e

     , MonadState st m -- Read-only
     , HasType Conf.Config st
     )
  => TeXParseT ExpandingStream m (Lex.Token, ResolvedToken)
pTakeAndResolveLexToken = TeXParseT takeAndResolveLexTokenS

takeAndResolveLexTokenS
  :: ( MonadError e m
     , AsType ResolutionError e

     , MonadState st m -- Read-only
     , HasType Conf.Config st
     )
  => ExpandingStream
  -> m (ExpandingStream, Either ParseError (Lex.Token, ResolvedToken))
takeAndResolveLexTokenS s =
  fetchResolvedToken s <&> \case
    Nothing ->
      (s, Left EndOfInput)
    Just (lt, rt, s') ->
      (s', Right (lt, rt))

instance TeXStream ExpandingStream where

  resolutionModeLens = G.P.typed @ResolutionMode

  tokenSourceLens = G.P.typed @(L.NE.NonEmpty TokenSource)

  lexStateLens = G.P.typed @Lex.LexState

  conditionBodyStateLens = G.P.typed @[ConditionBodyState]

-- Expanding syntax commands.
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

skipToIfDelim :: forall m. MonadTeXParse m => IfBodyState -> m ()
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
   . ( MonadState st m -- Read-only
     , HasType Conf.Config st

     , MonadTeXParse m

     , MonadError e m
     , AsTeXParseErrors e
     , AsType ExpansionError e
     )
  => ConditionTok
  -> m ()
expandConditionToken = \case
  ConditionHeadTok ifTok -> do
    condHead <- conditionHeadParser ifTok
    texEvaluate condHead >>= \case
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

     , MonadTeXParse m

     , MonadError e m
     , AsTeXParseErrors e
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
    conf <- gets $ getTyped @Conf.Config
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
    (_, postArgLTs) <- takeAndExpandResolvedToken
    -- Prepend the unexpanded token.
    pure (argLT <| postArgLTs)
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
    fmap charCodeAsMadeToken <$> texEvaluate intQuant
  ChangeCaseTok direction -> do
    conf <- gets $ getTyped @Conf.Config
    expandChangeCase
      (\c -> Conf.lookupChangeCaseCode direction c conf)
      <$> parseGeneralText

pTakeAndExpandResolvedToken
  :: ( MonadIO m

     , MonadState st m
     , HasType Conf.Config st

     , MonadTeXParse m

     , MonadError e m
     , AsTeXParseErrors e
     , AsType ExpansionError e
     )
  => m (ResolvedToken, Seq Lex.Token)
pTakeAndExpandResolvedToken = do
  (lt, rt) <- takeAndResolveLexToken
  case rt of
    PrimitiveToken _ ->
      pure (rt, singleton lt)
    SyntaxCommandHeadToken c -> do
      lts <- expandSyntaxCommand c
      pure (rt, lts)
