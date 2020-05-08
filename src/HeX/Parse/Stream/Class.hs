{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.Class where

import qualified Optics as O
import Optics.Cons ()
import qualified Control.Monad.Combinators as PC
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Generics.Product.Typed as G.P
import qualified Data.List.NonEmpty as L.NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy.Encoding as Tx.L.Enc
import Hex.Config (Config, ConfigError, lookupCS, lookupCatCode)
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import Hex.Lex (CharCat (..))
import qualified Hex.Lex as Lex
import Hex.Resolve
import Hexlude hiding (many)
import Path (Abs, File, Path)
import Control.Monad.Trans.Class

class TeXStream s where

  resolutionModeLens :: Lens' s ResolutionMode

  tokenSourceLens :: Lens' s (L.NE.NonEmpty TokenSource)

  lexStateLens :: Lens' s Lex.LexState

  conditionBodyStateLens :: Lens' s [ConditionBodyState]

data TokenSource
  = TokenSource
      { sourcePath :: Maybe (Path Abs File)
      , sourceCharCodes :: BS.L.ByteString
      , sourceLexTokens :: Seq Lex.Token
      }
  deriving stock (Show, Generic)

-- Lens for the head of a non-empty list.
neHeadL :: Lens' (L.NE.NonEmpty a) a
neHeadL = O.lens L.NE.head $ \(_ :| xs) x -> x :| xs

instance Describe TokenSource where

  describe TokenSource {sourcePath, sourceCharCodes, sourceLexTokens} =
      [ (0, "TokenSource")
      ,   (1, "path " <> quote (show sourcePath))
      ,   (1, "codes")
      ,     (2, toStrict (Tx.L.Enc.decodeUtf8 (BS.L.take 50 sourceCharCodes)))
      ]
      <> describeNamedRelFoldable1 "lexTokens" (Seq.take 10 sourceLexTokens)

newTokenSource :: Maybe (Path Abs File) -> BS.L.ByteString -> TokenSource
newTokenSource maybePath cs = TokenSource maybePath cs mempty

newtype ExpansionError = ExpansionError Text
  deriving stock Show

newtype ResolutionError = ResolutionError Text
  deriving stock Show

newtype ParseError = ParseError Text
  deriving stock Show

insertLexToken :: TeXStream b => b -> Lex.Token -> b
insertLexToken s t =
  s & tokenSourceLens % neHeadL % G.P.typed @(Seq Lex.Token) %~ O.cons t

type AsTeXParseErrors e
  = ( AsType EvaluationError e
    , AsType ConfigError e
    , AsType ResolutionError e
    )

class MonadPlus m => MonadTeXParse m where

  parseError :: ParseError -> m a

  satisfyThen :: (PrimitiveToken -> Maybe a) -> m a

  withInhibition :: m a -> m a

  takeWhileP :: (PrimitiveToken -> Bool) -> m (Seq PrimitiveToken)

  takeResolvedToken :: m ResolvedToken

  pushSkipState :: ConditionBodyState -> m ()

  peekSkipState :: m (Maybe ConditionBodyState)

  popSkipState :: m (Maybe ConditionBodyState)

satisfyIf :: MonadTeXParse m => (PrimitiveToken -> Bool) -> m PrimitiveToken
satisfyIf f = satisfyThen (\x -> if f x then Just x else Nothing)

anySingle :: MonadTeXParse m => m PrimitiveToken
anySingle = satisfyIf (const True)


type TeXParseCtx st e m
  = ( MonadState st m -- Read-only
    , HasType Config st

    , MonadTeXParse m

    , MonadError e m
    , AsTeXParseErrors e
    )

type TeXParseOutsideCtx s st e m
  = ( MonadState st m -- Read-only
    , HasType Config st

    , MonadTeXParse (TeXParseT s m)

    , MonadError e m
    , AsTeXParseErrors e
    , AsType ParseError e
    )

newtype TeXParseT s m a = TeXParseT (s -> m (s, Either ParseError a))

instance Functor m => Functor (TeXParseT s m) where
  fmap f (TeXParseT parse) = TeXParseT $ \s -> parse s <&> \(s', errOrA) ->
    case errOrA of
      Left e -> (s', Left e)
      Right a -> (s', Right (f a))

instance Monad m => Applicative (TeXParseT s m) where
  pure a = TeXParseT $ \s -> pure (s, Right a)

  -- (<*>) :: m (a -> b) -> m a -> m b
  (TeXParseT parseAToB) <*> (TeXParseT parseA) = TeXParseT $ \s -> do
    (s', errOrAToB) <- parseAToB s
    case errOrAToB of
      Left err ->
        -- s, not s' or s'': Backtracking.
        pure (s, Left err)
      Right aToB -> do
        (s'', errOrA) <- parseA s'
        case errOrA of
          Left err ->
            -- s, not s' or s'': Backtracking.
            pure (s, Left err)
          Right a ->
            pure (s'', Right $ aToB a)

instance Monad m => Monad (TeXParseT s m) where
  return = pure

  -- m a -> (a -> m b) -> m b
  (TeXParseT parseA) >>= aToTParseB = TeXParseT $ \s -> do
    (s', errOrA) <- parseA s
    case errOrA of
      Left err ->
        -- s, not s': Backtracking.
        pure (s, Left err)
      Right a ->
        let (TeXParseT parseB) = aToTParseB a
        in parseB s'

instance Monad m => Alternative (TeXParseT s m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (TeXParseT s m) where
  mzero = TeXParseT $ \s -> pure (s, Left $ ParseError "mzero")

  -- m a -> m a -> m a
  mplus (TeXParseT parseA1) (TeXParseT parseA2) = TeXParseT $ \s -> do
    (s1, errOrA1) <- parseA1 s
    case errOrA1 of
      Left err1 -> do
        -- s, not s1: Backtracking.
        (s2, errOrA2) <- parseA2 s
        case errOrA2 of
          Left _ ->
            -- err1, not err2: Report first failure error
            pure (s1, Left err1)
          Right a2 ->
            pure (s2, Right a2)
      Right a1 ->
        pure (s1, Right a1)

instance MonadTrans (TeXParseT s) where
  lift m = TeXParseT $ \s -> do
    a <- m
    pure (s, Right a)

instance (MonadError e m) => MonadError e (TeXParseT s m) where
  throwError = lift . throwError

  -- catchError :: TeXParseT s m a -> (e -> TeXParseT s m a) -> TeXParseT s m a
  -- catchError :: m a -> (e -> m a) -> m a
  catchError (TeXParseT parseA) errToHandle = TeXParseT $ \s ->
    catchError (parseA s) $ \e -> do
      let (TeXParseT parseRecover) = errToHandle e
      parseRecover s

instance MonadState st m => MonadState st (TeXParseT s m) where
  get = lift get
  put = lift . put

runTeXParseT
  :: TeXParseT s m a
  -> s
  -> m (s, Either ParseError a)
runTeXParseT (TeXParseT f) = f

runTeXParseTEmbedded
  :: ( MonadError e m
     , AsType ParseError e
     )
  => TeXParseT s m a
  -> s
  -> m (s, a)
runTeXParseTEmbedded p s = do
  (s', errOrV) <- runTeXParseT p s
  case errOrV of
    Left err ->
      throwError $ injectTyped err
    Right v ->
      pure (s', v)

insertLexTokens :: TeXStream s => s -> Seq Lex.Token -> s
insertLexTokens s (ts :|> t) = insertLexTokens (insertLexToken s t) ts
insertLexTokens s Empty = s

manySatisfiedIf :: MonadTeXParse m => (PrimitiveToken -> Bool) -> m [PrimitiveToken]
manySatisfiedIf testTok = PC.many $ satisfyIf testTok

manySatisfiedThen :: MonadTeXParse m => (PrimitiveToken -> Maybe a) -> m [a]
manySatisfiedThen f = PC.many $ satisfyThen f

-- Skipping.
skipSatisfied :: MonadTeXParse m => (PrimitiveToken -> Bool) -> m ()
skipSatisfied f = void (satisfyIf f)

satisfyEquals :: MonadTeXParse m => PrimitiveToken -> m ()
satisfyEquals t = skipSatisfied (== t)

skipOptional :: MonadTeXParse m => m a -> m ()
skipOptional p = void (optional p)

skipOneOptionalSatisfied :: MonadTeXParse m => (PrimitiveToken -> Bool) -> m ()
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: MonadTeXParse m => (PrimitiveToken -> Bool) -> m ()
skipManySatisfied = PC.skipMany . skipSatisfied

skipSatisfiedChunk :: MonadTeXParse m => Seq PrimitiveToken -> m ()
skipSatisfiedChunk = foldr (satisfyEquals >>> (>>)) (pure ())

choiceFlap :: MonadTeXParse m => [PrimitiveToken -> m a] -> PrimitiveToken -> m a
choiceFlap headsToParsers t =
  PC.choice (flap headsToParsers t)

parseHeaded :: MonadTeXParse m => (PrimitiveToken -> m a) -> m a
parseHeaded = (anySingle >>=)

-- Inhibition.
inhibitResolution, enableResolution :: TeXStream s => s -> s
inhibitResolution = resolutionModeLens .~ NotResolving

enableResolution = resolutionModeLens .~ Resolving

  -- P.updateParserState (\st@P.State {P.stateInput} -> st {P.stateInput = inhibitResolution stateInput})
  -- v <- p
  -- P.updateParserState (\st@P.State {P.stateInput} -> st {P.stateInput = enableResolution stateInput})
  -- pure v

-- Cases where expansion is inhibited:
-- 1.  While deleting tokens during error recovery
-- 2.  While skipping tokens because conditional text is being ignored
-- 3.  While reading macro arguments
-- 4.  While reading a control sequence to be defined by \let, \futurelet,
--     \def, \gdef, \edef, \xdef, \chardef, \mathchardef, \countdef, \dimendef,
--     \skipdef, \muskipdef, \toksdef, \read or \font
-- 5.  While reading argument tokens for \expandafter, \noexpand, \string,
--     \meaning, \let, \futurelet, \ifx, \show, \afterassignment or \aftergroup
-- 6.  While absorbing the parameter text of a \def, \gdef, \edef or \xdef
-- 7.  While absorbing the replacement text of a \def, \gdef or \read; or the
--     text of a token variable like \everypar or \toks0; or the token list for
--     \uppercase or \lowercase or \write. (The token list for \write will be
--     expanded later, when it is actually output to a file.)
-- 8.  While reading the preamble of an alignment, except after a token for the
--     primitive command \span or when reading the ⟨glue⟩ after \tabskip
-- 9.  Just after a <$,3> token that begins math mode, to see if another $3
--     follows
-- 10.  Just after a <‘,12> token that begins an alphabetic constant
-- Case 3, macro arguments.
newtype MacroArgument = MacroArgument (Seq Lex.Token)
  deriving stock (Show, Eq)

nrExpressions :: Foldable t => (a -> Ordering) -> t a -> Maybe (Int, Int)
nrExpressions f = foldM next (0, 0)
  where
    next v@(dpth, nrExprs) x = case f x of
      EQ -> Just v
      GT -> Just (succ dpth, nrExprs)
      LT
        | dpth < 1 -> Nothing
        | dpth == 1 -> Just (pred dpth, succ nrExprs)
        | otherwise -> Just (pred dpth, nrExprs)

hasValidGrouping :: Foldable t => (a -> Ordering) -> t a -> Bool
hasValidGrouping f xs = case nrExpressions f xs of
  Just (0, _) -> True
  _ -> False

splitLast :: Seq a -> Maybe (Seq a, a)
splitLast = \case
  xs :|> x -> Just (xs, x)
  _ -> Nothing

unsafeParseMacroArgs :: forall m. MonadTeXParse m => MacroContents -> m (Map.Map Digit MacroArgument)
unsafeParseMacroArgs MacroContents {preParamTokens = pre, parameters = params} = do
  skipBalancedText pre
  parseArgs params
  where
    parseArgs :: MacroParameters -> m (Map.Map Digit MacroArgument)
    parseArgs ps = case Map.minViewWithKey ps of
      -- If there are no parameters, expect no arguments.
      Nothing -> pure Map.empty
      Just ((dig, p), rest) -> do
        argRaw <-
          case p of
            BalancedText Empty -> parseUndelimitedArgs
            BalancedText delims -> parseDelimitedArgs Empty delims
        -- If the argument has the form ‘{⟨nested tokens⟩}’, where ⟨nested
        -- tokens⟩ stands for any properly nested token sequence, the outermost
        -- braces are removed.
        -- If appropriate, strip the argument; otherwise use the unstripped
        -- version.
        let arg = fromMaybe argRaw (getStripped argRaw)
        Map.insert dig (MacroArgument arg) <$> parseArgs rest
    -- If the parameter is undelimited, the argument is the next non-blank
    -- token, unless that token is ‘{’, when the argument will be the entire
    -- following {...} group.
    parseUndelimitedArgs :: m (Seq Lex.Token)
    parseUndelimitedArgs = do
      -- Skip blank tokens (assumed to mean spaces).
      skipManySatisfied (primTokHasCategory Code.Space)
      unsafeAnySingleLex >>= \case
        t@(Lex.CharCatToken CharCat {cat = Code.BeginGroup}) -> do
          BalancedText ts <- unsafeParseBalancedText Include
          pure $ t :<| ts
        t -> pure $ singleton t
    -- Get the shortest, possibly empty, properly nested sequence of tokens,
    -- followed by the delimiter tokens. In the delimiter, category codes,
    -- character codes and control sequence names must match.
    parseDelimitedArgs :: Seq Lex.Token -> Seq Lex.Token -> m (Seq Lex.Token)
    parseDelimitedArgs ts delims = do
      -- Parse tokens until we see the delimiter tokens, then add what we grab
      -- to our accumulating argument.
      newAcc <- Seq.fromList <$> PC.manyTill unsafeAnySingleLex (skipSatisfiedLexChunk delims)
      let arg = ts <> newAcc
      if hasValidGrouping tokToChange arg
      then-- If the argument has valid grouping, then we are done.
        pure arg
      else-- Otherwise, add the 'red herring' delimiters we just parsed and
      -- continue.
        parseDelimitedArgs (arg <> delims) delims
    -- Check if an argument has an outer '{}' pair that should be stripped, and
    -- do this if so.
    -- If we got an empty argument, can consider this to 'strip' to itself.
    getStripped Empty = Nothing
    getStripped (a :<| xs) = do
      -- First token must be a '{'.
      guard $ lexTokHasCategory Code.BeginGroup a
      -- Must have at least two tokens. If so, get the last token, 'z', and the
      -- tokens that sit before it, i.e. the stripped argument.
      (inner, z) <- splitLast xs
      -- The last token must be a '}'.
      guard $ lexTokHasCategory Code.EndGroup z
      -- The stripped argument must have valid grouping.
      guard $ hasValidGrouping tokToChange inner
      -- Return the stripped argument.
      pure inner

-- Case 4, for things like 'macroName' in '\def\macroName'.
unsafeParseCSName :: MonadTeXParse m => m Lex.ControlSequenceLike
unsafeParseCSName = handleLex tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken CharCat {cat = Code.Active, char = c}) =
      Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) =
      Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

-- Case 5, arbitrary tokens such as for \let\foo=<token>.
unsafeAnySingleLex :: MonadTeXParse m => m Lex.Token
unsafeAnySingleLex = satisfyThen tokToLex

-- Case 6, macro parameter text.
-- Trivially balanced, because no braces are allowed at all.
parseParamDelims :: MonadTeXParse m => m BalancedText
parseParamDelims = BalancedText . Seq.fromList <$> manySatisfiedThen tokToDelimTok
  where
    tokToDelimTok = \case
      UnresolvedTok lt@(Lex.CharCatToken CharCat {cat}) -> case cat of
        Code.Parameter -> Nothing
        Code.BeginGroup -> Nothing
        Code.EndGroup -> Nothing
        _ -> Just lt
      UnresolvedTok lt ->
        Just lt
      _ -> Nothing

headToMaybeParseParametersFrom :: MonadTeXParse m => Digit -> PrimitiveToken -> m MacroParameters
headToMaybeParseParametersFrom dig _t =
  headToParseEndOfParams _t <|> headToParseParamsFrom _t
  where
    -- Parse the left-brace that indicates the end of parameters.
    headToParseEndOfParams t
      | primTokHasCategory Code.BeginGroup t =
        pure Map.empty
      | otherwise =
        empty
    -- Parse a present parameter, then the remaining parameters, if present.
    -- Parse, for example, '#3'.
    headToParseParamsFrom t
      | primTokHasCategory Code.Parameter t =
        do
          skipSatisfied $ \case
            UnresolvedTok (Lex.CharCatToken CharCat {char, cat}) -> case cat of
              Code.Letter ->
                char == digitToChar dig
              Code.Other ->
                char == digitToChar dig
              _ -> False
            _ -> False
          -- Parse delimiter tokens after the parameter number, if present.
          thisParam <- parseParamDelims
          -- Return this parameter, plus any remaining parameters.
          Map.insert dig thisParam <$> case dig of
            -- If we are parsing parameter nine, there can't be any more, so we
            -- only expect to end the parameters.
            Nine -> parseHeaded headToParseEndOfParams
            -- Otherwise, we can either end the parameters, or have some more,
            -- starting from the successor of this digit.
            _ -> parseHeaded $ headToMaybeParseParametersFrom (succ dig)
      | otherwise =
        empty

unsafeParseParamText :: MonadTeXParse m => m (BalancedText , MacroParameters)
unsafeParseParamText = do
  -- Pre-parameter text tokens.
  preParamToks <- parseParamDelims
  -- Parameters, if present.
  params <- parseHeaded $ headToMaybeParseParametersFrom minBound
  pure (preParamToks, params)

-- Case 7, general lists of token.
-- How to handle a terminal item.
data TerminusPolicy = Include | Discard
  deriving stock (Show, Eq)

-- Nested expression with valid grouping.
parseNestedExpr :: MonadTeXParse m => m (a, Ordering) -> TerminusPolicy -> m (Seq a)
parseNestedExpr parseNext policy = go mempty (1 :: Int)
  where
    go acc = \case
      0 -> pure acc
      depth -> do
        (x, change) <- parseNext
        -- Get next stack depth.
        let nextDepth = case change of
              LT -> pred depth
              GT -> succ depth
              EQ -> depth
        case nextDepth of
          -- When we reach zero depth, we are done.
          -- Catch it early, rather than recursing, to avoid returning the final ')',
          -- because we don't want it.
          0 ->
            pure $ case policy of
              Include -> acc |> x
              Discard -> acc
          -- Otherwise, append our result and continue.
          _ -> go (acc |> x) nextDepth

-- Part of case 7, \uppercase's body and such.
tokToChange :: Lex.Token -> Ordering
tokToChange t
  | lexTokHasCategory Code.BeginGroup t = GT
  | lexTokHasCategory Code.EndGroup t = LT
  | otherwise = EQ

-- This assumes we just parsed the '{' that starts the balanced text.
unsafeParseBalancedText :: MonadTeXParse m => TerminusPolicy -> m BalancedText
unsafeParseBalancedText policy =
  BalancedText <$> parseNestedExpr parseNext policy
  where
    parseNext = handleLex $ \t -> Just (t, tokToChange t)

-- Part of case 7, macro replacement text.
-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.
-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like unsafeParseBalancedText, but extracts argument calls.
unsafeParseMacroText :: MonadTeXParse m => m MacroText
unsafeParseMacroText = MacroText <$> parseNestedExpr parseNext Discard
  where
    parseNext =
      unsafeAnySingleLex >>= \case
        -- If we see a '#', parse the parameter number and return a token
        -- representing the call.
        Lex.CharCatToken CharCat {cat = Code.Parameter} ->
          handleLex handleParamNr <&> (,EQ)
        -- Otherwise, just return the ordinary lex token.
        t -> pure (MacroTextLexToken t, tokToChange t)
    -- We are happy iff the '#' is followed by a decimal digit, or another '#'.
    handleParamNr = \case
      Lex.CharCatToken cc@CharCat {char = c, cat = cat}
        | cat `elem` [Code.Letter, Code.Other] ->
          MacroTextParamToken <$> charCodeToDigit c
        | cat == Code.Parameter ->
          pure $ MacroTextLexToken $ Lex.CharCatToken cc {cat = Code.Other}
      _ ->
        Nothing

-- Case 10, character constant like "`c".
unsafeParseCharLike :: MonadTeXParse m => m Code.CharCode
unsafeParseCharLike = handleLex tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken CharCat {char = c}) =
      Just c
    tokToCharLike (Lex.ControlSequenceToken Lex.ControlSequence {Lex.csChars = c :<| Empty}) =
      Just c
    tokToCharLike _ =
      Nothing

-- Interface.
parseBalancedText :: MonadTeXParse m => TerminusPolicy -> m BalancedText
parseBalancedText = withInhibition . unsafeParseBalancedText

parseMacroArgs :: MonadTeXParse m => MacroContents -> m (Map.Map Digit MacroArgument)
parseMacroArgs = withInhibition . unsafeParseMacroArgs

parseCharLike :: MonadTeXParse m => m Code.CharCode
parseCharLike = withInhibition unsafeParseCharLike

parseCSName :: MonadTeXParse m => m Lex.ControlSequenceLike
parseCSName = withInhibition unsafeParseCSName

parseParamText :: MonadTeXParse m => m (BalancedText, MacroParameters)
parseParamText = withInhibition unsafeParseParamText

parseMacroText :: MonadTeXParse m => m MacroText
parseMacroText = withInhibition unsafeParseMacroText

parseLexToken :: MonadTeXParse m => m Lex.Token
parseLexToken = withInhibition unsafeAnySingleLex

parseLetArg :: MonadTeXParse m => m Lex.Token
parseLetArg = withInhibition skipOneOptionalSpace >> parseLexToken

-- Derived related parsers.
skipFiller :: MonadTeXParse m => m ()
skipFiller = void $ takeWhileP isFillerItem

parseExpandedBalancedText :: MonadTeXParse m => TerminusPolicy -> m ExpandedBalancedText
parseExpandedBalancedText policy =
  ExpandedBalancedText <$> parseNestedExpr parseNext policy
  where
    parseNext = satisfyThen $ \pt -> Just (pt, primTokToChange pt)
    primTokToChange = \case
      UnresolvedTok lt -> tokToChange lt
      _ -> EQ

_parseDelimitedText :: MonadTeXParse m => (TerminusPolicy -> m a) -> m a
_parseDelimitedText parser = do
  skipFiller
  skipLeftBrace
  parser Discard

parseGeneralText :: MonadTeXParse m => m BalancedText
parseGeneralText = _parseDelimitedText parseBalancedText

parseExpandedGeneralText :: MonadTeXParse m => m ExpandedBalancedText
parseExpandedGeneralText = _parseDelimitedText parseExpandedBalancedText

-- Helpers.
ccHasCategory :: Code.CoreCatCode -> CharCat -> Bool
ccHasCategory a CharCat {cat = b} = a == b

lexTokHasCategory :: Code.CoreCatCode -> Lex.Token -> Bool
lexTokHasCategory a (Lex.CharCatToken cc) = ccHasCategory a cc
lexTokHasCategory _ _ = False

primTokHasCategory :: Code.CoreCatCode -> PrimitiveToken -> Bool
primTokHasCategory a (UnresolvedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory Code.Space

-- Match particular tokens.
isFillerItem :: PrimitiveToken -> Bool
isFillerItem = \case
  RelaxTok -> True
  t -> isSpace t

matchOtherToken :: Char -> PrimitiveToken -> Bool
matchOtherToken c2 = \case
  UnresolvedTok (Lex.CharCatToken CharCat {cat = Code.Other, char = c1}) ->
    c1 == Code.CharCode_ c2
  _ ->
    False

matchNonActiveCharacterUncased :: Code.CharCode -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a = \case
  UnresolvedTok (Lex.CharCatToken CharCat {char, cat}) ->
    (cat /= Code.Active) && (char == Code.toUpperChar a || char == Code.toLowerChar a)
  _ ->
    False

tokToChar :: PrimitiveToken -> Maybe Code.CharCode
tokToChar = \case
  UnresolvedTok (Lex.CharCatToken CharCat {char}) ->
    Just char
  _ ->
    Nothing

-- Lexed.
tokToLex :: PrimitiveToken -> Maybe Lex.Token
tokToLex = \case
  UnresolvedTok t -> Just t
  _ -> Nothing

handleLex :: MonadTeXParse m => (Lex.Token -> Maybe a) -> m a
handleLex f = satisfyThen $ tokToLex >=> f

satisfyEqualsLex :: MonadTeXParse m => Lex.Token -> m ()
satisfyEqualsLex lt = void $ satisfyEquals (UnresolvedTok lt)

skipSatisfiedLexChunk :: MonadTeXParse m => Seq Lex.Token -> m ()
skipSatisfiedLexChunk ts = skipSatisfiedChunk (UnresolvedTok <$> ts)

skipBalancedText :: MonadTeXParse m => BalancedText -> m ()
skipBalancedText (BalancedText toks) = skipSatisfiedLexChunk toks

-- Parsers.
skipOneOptionalSpace :: MonadTeXParse m => m ()
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- TODO: Maybe other things can act as left braces.
skipLeftBrace :: MonadTeXParse m => m ()
skipLeftBrace = skipSatisfied $ primTokHasCategory Code.BeginGroup

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: MonadTeXParse m => m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: MonadTeXParse m => m ()
skipOptionalEquals = skipOptionalSpaces >> skipOneOptionalSatisfied (matchOtherToken '=')

skipKeyword :: MonadTeXParse m => [Code.CharCode] -> m ()
skipKeyword s =
  skipOptionalSpaces >>
    mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: MonadTeXParse m => [Code.CharCode] -> m Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

parseManyChars :: MonadTeXParse m => m [Code.CharCode]
parseManyChars = PC.many $ satisfyThen tokToChar

withJust :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
withJust a k =
  a >>= \case
    Nothing -> pure Nothing
    Just x -> k x

fetchResolvedToken
  :: ( MonadError e m
     , AsType ResolutionError e
     , TeXStream s

     , MonadState st m -- Read-only
     , HasType Config st
     )
  => s
  -> m (Maybe (Lex.Token, ResolvedToken, s))
fetchResolvedToken stream = do
  conf <- gets $ getTyped @Config
  let lkpCatCode t = lookupCatCode t conf
  let lkpCS cs = lookupCS cs conf
  extractResolvedToken stream lkpCatCode lkpCS

extractResolvedToken
  :: ( MonadError e m
     , AsType ResolutionError e
     , TeXStream s
     )
  => s
  -> (Code.CharCode -> Code.CatCode)
  -> (Lex.ControlSequenceLike -> Maybe ResolvedToken)
  -> m (Maybe (Lex.Token, ResolvedToken, s))
extractResolvedToken stream lkpCatCode lkpCS =
  case extractLexToken stream lkpCatCode of
    Nothing -> pure Nothing
    Just (lt, newStream) -> do
      rt <-
        note
          (injectTyped $ ResolutionError $ "Could not resolve lex token: " <> renderDescribed lt) $
          resolveToken lkpCS (newStream ^. resolutionModeLens) lt
      pure $ Just (lt, rt, newStream)

fetchLexToken
  :: ( TeXStream s

     , MonadState st m -- Read-only
     , HasType Config st
     )
  => s
  -> m (Maybe (Lex.Token, s))
fetchLexToken stream = do
  conf <- gets (getTyped @Config)
  let lkpCatCode t = lookupCatCode t conf
  pure $ extractLexToken stream lkpCatCode

extractLexToken :: TeXStream s => s -> (Code.CharCode -> Code.CatCode) -> Maybe (Lex.Token, s)
extractLexToken stream lkpCatCode = do
  (lt, newLexState, newStreamTokenSources) <- extractFromSources (stream ^. tokenSourceLens)
  pure
    ( lt
    , stream &
        tokenSourceLens .~ newStreamTokenSources &
        lexStateLens .~ newLexState
    )
  where
    curLexState = stream ^. lexStateLens

    -- TODO:
    -- [a] -> (a -> Maybe b) -> Maybe (b, [a])
    extractFromSources (curTokSource :| outerTokSources) = case extractFromSource curTokSource of
      Nothing ->
        nonEmpty outerTokSources >>= extractFromSources
      Just (lt, lexState, newCurTokSource) ->
        Just $ seq outerTokSources (lt, lexState, newCurTokSource :| outerTokSources)

    extractFromSource tokSource@TokenSource {sourceCharCodes, sourceLexTokens} = case sourceLexTokens of
      -- If there is a lex token in the buffer, use that.
      fstLexToken :<| laterLexTokens ->
        let newCurTokSource = tokSource {sourceLexTokens = laterLexTokens}
        in Just (fstLexToken, curLexState, newCurTokSource)
      -- If the lex token buffer is empty, extract a token and use it.
      Empty ->
        Lex.extractToken lkpCatCode curLexState sourceCharCodes
          <&> \(extractedLexToken, newLexState, newCodes) ->
            (extractedLexToken, newLexState, tokSource {sourceCharCodes = newCodes})
