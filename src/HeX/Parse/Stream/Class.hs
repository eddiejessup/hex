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
import qualified Text.Megaparsec as P

class TeXStream s where

  resolutionModeLens :: Lens' s ResolutionMode

  tokenSourceLens :: Lens' s (L.NE.NonEmpty TokenSource)

  lexStateLens :: Lens' s Lex.LexState

  getConditionBodyState :: s -> Maybe ConditionBodyState

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
    , AsType ParseError e
    , AsType ResolutionError e
    )

type SimpleParsecT s m a = P.ParsecT Void s m a

type TeXParseable s st e m
  = ( Monad m
    , MonadState st m -- Read-only
    , HasType Config st

    , MonadError e m
    , AsTeXParseErrors e

    , P.Stream s m
    , P.Token s ~ PrimitiveToken
    , TeXStream s
    , Describe s
    )

type TeXParser s st e m a = TeXParseable s st e m => SimpleParsecT s m a

simpleRunParserT'
  :: Monad m
  => P.ParsecT e s m a
  -> s
  -> m (s, Either (P.ParseErrorBundle s e) a)
simpleRunParserT' parser stream = do
  (P.State {P.stateInput = resultStream}, v) <- P.runParserT' parser inState
  pure (resultStream, v)
  where
    inState = P.State
      { P.stateInput = stream
      , P.stateOffset = 1
      , P.statePosState = P.PosState
        { P.pstateInput = stream
        , P.pstateOffset = 1
        , P.pstateSourcePos = P.SourcePos
          { P.sourceName = "TeX file"
          , P.sourceLine = P.mkPos 1
          , P.sourceColumn = P.mkPos 1
          }
        , P.pstateTabWidth = P.mkPos 4
        , P.pstateLinePrefix = "TeX Error = "
        }
      }

runSimpleRunParserT'
  :: ( MonadError e m
     , AsType ParseError e
     , Describe (P.Token s)
     )
  => P.ParsecT Void s m a
  -> s
  -> m (s, a)
runSimpleRunParserT' parser stream =
  simpleRunParserT' parser stream >>= \case
    (resultStream, Right a) ->
      pure (resultStream, a)
    (_, Left err) ->
      throwError $ injectTyped $ ParseError $ renderDescribed err

insertLexTokens :: TeXStream s => s -> Seq Lex.Token -> s
insertLexTokens s (ts :|> t) = insertLexTokens (insertLexToken s t) ts
insertLexTokens s Empty = s

satisfyThen :: (P.Token s -> Maybe a) -> TeXParser s st e m a
satisfyThen test = P.token test mempty

type MatchToken s = P.Token s -> Bool

manySatisfied :: MatchToken s -> TeXParser s st e m [P.Token s]
manySatisfied testTok = PC.many $ P.satisfy testTok

manySatisfiedThen :: (P.Token s -> Maybe a) -> TeXParser s st e m [a]
manySatisfiedThen f = PC.many $ satisfyThen f

-- Skipping.
skipSatisfied :: P.MonadParsec e s m => MatchToken s -> m ()
skipSatisfied f = void (P.satisfy f)

satisfyEquals :: P.MonadParsec e s m => P.Token s -> m ()
satisfyEquals t = skipSatisfied (== t)

skipOptional :: TeXParser s st e m a -> TeXParser s st e m ()
skipOptional p = void (optional p)

skipOneOptionalSatisfied :: MatchToken s -> TeXParser s st e m ()
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: MatchToken s -> TeXParser s st e m ()
skipManySatisfied = PC.skipMany . skipSatisfied

-- skipManySatisfied = void . P.takeWhileP Nothing
skipSatisfiedChunk :: Seq (P.Token s) -> TeXParser s st e m ()
skipSatisfiedChunk = foldr (satisfyEquals >>> (>>)) (pure ())

choiceFlap :: P.MonadParsec e s m => [P.Token s -> m a] -> P.Token s -> m a
choiceFlap headsToParsers t =
  PC.choice (flap headsToParsers t)

parseHeaded :: P.MonadParsec e s m => (P.Token s -> m a) -> m a
parseHeaded = (P.anySingle >>=)

-- Inhibition.
inhibitResolution, enableResolution :: TeXStream s => s -> s
inhibitResolution = resolutionModeLens .~ NotResolving

enableResolution = resolutionModeLens .~ Resolving

parseInhibited :: TeXParser s st e m a -> TeXParser s st e m a
parseInhibited p = do
  P.updateParserState (\st@P.State {P.stateInput} -> st {P.stateInput = inhibitResolution stateInput})
  v <- p
  P.updateParserState (\st@P.State {P.stateInput} -> st {P.stateInput = enableResolution stateInput})
  pure v

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

unsafeParseMacroArgs :: MacroContents -> TeXParser s st e m (Map.Map Digit MacroArgument)
unsafeParseMacroArgs MacroContents {preParamTokens = pre, parameters = params} = do
  skipBalancedText pre
  parseArgs params
  where
    parseArgs :: MacroParameters -> TeXParser s st e m (Map.Map Digit MacroArgument)
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
    parseUndelimitedArgs :: TeXParser s st e m (Seq Lex.Token)
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
    parseDelimitedArgs :: Seq Lex.Token -> Seq Lex.Token -> TeXParser s st e m (Seq Lex.Token)
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
unsafeParseCSName :: TeXParser s st e m Lex.ControlSequenceLike
unsafeParseCSName = handleLex tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken CharCat {cat = Code.Active, char = c}) =
      Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) =
      Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

-- Case 5, arbitrary tokens such as for \let\foo=<token>.
unsafeAnySingleLex :: TeXParser s st e m Lex.Token
unsafeAnySingleLex = satisfyThen tokToLex

-- Case 6, macro parameter text.
-- Trivially balanced, because no braces are allowed at all.
parseParamDelims :: TeXParser s st e m BalancedText
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

headToMaybeParseParametersFrom :: Digit -> PrimitiveToken -> TeXParser s st e m MacroParameters
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

unsafeParseParamText
  :: TeXParser s st e m
       ( BalancedText
       , MacroParameters
       )
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
parseNestedExpr :: TeXParser s st e m (a, Ordering) -> TerminusPolicy -> TeXParser s st e m (Seq a)
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
unsafeParseBalancedText :: TerminusPolicy -> TeXParser s st e m BalancedText
unsafeParseBalancedText policy =
  BalancedText <$> parseNestedExpr parseNext policy
  where
    parseNext = handleLex $ \t -> Just (t, tokToChange t)

-- Part of case 7, macro replacement text.
-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.
-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like unsafeParseBalancedText, but extracts argument calls.
unsafeParseMacroText :: TeXParser s st e m MacroText
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
unsafeParseCharLike :: TeXParser s st e m Code.CharCode
unsafeParseCharLike = handleLex tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken CharCat {char = c}) =
      Just c
    tokToCharLike (Lex.ControlSequenceToken Lex.ControlSequence {Lex.csChars = c :<| Empty}) =
      Just c
    tokToCharLike _ =
      Nothing

-- Interface.
parseBalancedText :: TerminusPolicy -> TeXParser s st e m BalancedText
parseBalancedText = parseInhibited . unsafeParseBalancedText

parseMacroArgs :: MacroContents -> TeXParser s st e m (Map.Map Digit MacroArgument)
parseMacroArgs = parseInhibited . unsafeParseMacroArgs

parseCharLike :: TeXParser s st e m Code.CharCode
parseCharLike = parseInhibited unsafeParseCharLike

parseCSName :: TeXParser s st e m Lex.ControlSequenceLike
parseCSName = parseInhibited unsafeParseCSName

parseParamText :: TeXParser s st e m (BalancedText, MacroParameters)
parseParamText = parseInhibited unsafeParseParamText

parseMacroText :: TeXParser s st e m MacroText
parseMacroText = parseInhibited unsafeParseMacroText

parseLexToken :: TeXParser s st e m Lex.Token
parseLexToken = parseInhibited unsafeAnySingleLex

parseLetArg :: TeXParser s st e m Lex.Token
parseLetArg = parseInhibited skipOneOptionalSpace >> parseLexToken

-- Derived related parsers.
skipFiller :: TeXParser s st e m ()
skipFiller = void $ P.takeWhileP Nothing isFillerItem

parseExpandedBalancedText :: TerminusPolicy -> TeXParser s st e m ExpandedBalancedText
parseExpandedBalancedText policy =
  ExpandedBalancedText <$> parseNestedExpr parseNext policy
  where
    parseNext = satisfyThen $ \pt -> Just (pt, primTokToChange pt)
    primTokToChange = \case
      UnresolvedTok lt -> tokToChange lt
      _ -> EQ

_parseDelimitedText
  :: (TerminusPolicy -> TeXParser s st e m a)
  -> TeXParser s st e m a
_parseDelimitedText parser = do
  skipFiller
  skipLeftBrace
  parser Discard

parseGeneralText :: TeXParser s st e m BalancedText
parseGeneralText = _parseDelimitedText parseBalancedText

parseExpandedGeneralText :: TeXParser s st e m ExpandedBalancedText
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

handleLex :: (Lex.Token -> Maybe a) -> TeXParser s st e m a
handleLex f = satisfyThen $ tokToLex >=> f

satisfyEqualsLex :: Lex.Token -> TeXParser s st e m ()
satisfyEqualsLex lt = void $ satisfyEquals (UnresolvedTok lt)

skipSatisfiedLexChunk :: Seq Lex.Token -> TeXParser s st e m ()
skipSatisfiedLexChunk ts = skipSatisfiedChunk (UnresolvedTok <$> ts)

skipBalancedText :: BalancedText -> TeXParser s st e m ()
skipBalancedText (BalancedText toks) = skipSatisfiedLexChunk toks

-- Parsers.
skipOneOptionalSpace :: TeXParser s st e m ()
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

tryChoice
  :: (Foldable f, Functor f, P.MonadParsec e s m)
  => f (m a)
  -> m a
tryChoice = PC.choice . (P.try <$>)

-- TODO: Maybe other things can act as left braces.
skipLeftBrace :: TeXParser s st e m ()
skipLeftBrace = skipSatisfied $ primTokHasCategory Code.BeginGroup

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: TeXParser s st e m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: TeXParser s st e m ()
skipOptionalEquals = skipOptionalSpaces >> skipOneOptionalSatisfied (matchOtherToken '=')

skipKeyword :: [Code.CharCode] -> TeXParser s st e m ()
skipKeyword s =
  skipOptionalSpaces >>
    mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: [Code.CharCode] -> TeXParser s st e m Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

parseManyChars :: TeXParser s st e m [Code.CharCode]
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
