{-# LANGUAGE RankNTypes #-}

module Hex.Parse.CommandParser.Inhibited where

import           Hexlude

import qualified Hex.Config.Codes       as Code
import           Hex.Resolve.Token
import qualified Control.Monad.Combinators as PC
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Hex.Lex (CharCat (..))
import qualified Hex.Lex as Lex
import Hex.Parse.TokenParser.Combinators
import Hex.Parse.TokenParser.Class

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

unsafeParseMacroArgs :: forall m. MonadTokenParse m => MacroContents -> m (Map.Map Digit MacroArgument)
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
unsafeParseCSName :: MonadTokenParse m => m Lex.ControlSequenceLike
unsafeParseCSName = handleLex tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken CharCat {cat = Code.Active, char = c}) =
      Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) =
      Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

-- Case 5, arbitrary tokens such as for \let\foo=<token>.
unsafeAnySingleLex :: MonadTokenParse m => m Lex.Token
unsafeAnySingleLex = satisfyThen tokToLex

-- Case 6, macro parameter text.
-- Trivially balanced, because no braces are allowed at all.
parseParamDelims :: MonadTokenParse m => m BalancedText
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

headToMaybeParseParametersFrom :: MonadTokenParse m => Digit -> PrimitiveToken -> m MacroParameters
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

unsafeParseParamText :: MonadTokenParse m => m (BalancedText , MacroParameters)
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
parseNestedExpr :: MonadTokenParse m => m (a, Ordering) -> TerminusPolicy -> m (Seq a)
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
unsafeParseBalancedText :: MonadTokenParse m => TerminusPolicy -> m BalancedText
unsafeParseBalancedText policy =
  BalancedText <$> parseNestedExpr parseNext policy
  where
    parseNext = handleLex $ \t -> Just (t, tokToChange t)

-- Part of case 7, macro replacement text.
-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.
-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like unsafeParseBalancedText, but extracts argument calls.
unsafeParseMacroText :: MonadTokenParse m => m MacroText
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

-- Interface.
parseBalancedText :: MonadTokenParse m => TerminusPolicy -> m BalancedText
parseBalancedText = withInhibition . unsafeParseBalancedText

parseMacroArgs :: MonadTokenParse m => MacroContents -> m (Map.Map Digit MacroArgument)
parseMacroArgs = withInhibition . unsafeParseMacroArgs

-- Case 10, character constant like "`c".
parseCharLike :: MonadTokenParse m => m Code.CharCode
parseCharLike =
  withInhibition $ handleLex $ \case
    Lex.CharCatToken CharCat {char = c} ->
      Just c
    Lex.ControlSequenceToken (Lex.ControlSequence bs) -> do
      -- If bytestring is empty, fail to parse.
      (c, rest) <- BS.uncons bs
      -- Succeed if rest is empty, i.e. whole thing is one word long.
      if BS.null rest
        then Just $ Code.CharCode c
        else Nothing

parseCSName :: MonadTokenParse m => m Lex.ControlSequenceLike
parseCSName = do
  traceM "parsing CS name"
  a <- withInhibition unsafeParseCSName
  traceM "parsed CS name"
  pure a

parseParamText :: MonadTokenParse m => m (BalancedText, MacroParameters)
parseParamText = withInhibition unsafeParseParamText

parseMacroText :: MonadTokenParse m => m MacroText
parseMacroText = withInhibition unsafeParseMacroText

parseLexToken :: MonadTokenParse m => m Lex.Token
parseLexToken = withInhibition unsafeAnySingleLex

parseLetArg :: MonadTokenParse m => m Lex.Token
parseLetArg = withInhibition skipOneOptionalSpace >> parseLexToken

-- Derived related parsers.
skipFiller :: MonadTokenParse m => m ()
skipFiller = void $ takeWhileP isFillerItem

parseExpandedBalancedText :: MonadTokenParse m => TerminusPolicy -> m ExpandedBalancedText
parseExpandedBalancedText policy =
  ExpandedBalancedText <$> parseNestedExpr parseNext policy
  where
    parseNext = satisfyThen $ \pt -> Just (pt, primTokToChange pt)
    primTokToChange = \case
      UnresolvedTok lt -> tokToChange lt
      _ -> EQ

_parseDelimitedText :: MonadTokenParse m => (TerminusPolicy -> m a) -> m a
_parseDelimitedText parser = do
  skipFiller
  skipLeftBrace
  parser Discard

parseGeneralText :: MonadTokenParse m => m BalancedText
parseGeneralText = _parseDelimitedText parseBalancedText

parseExpandedGeneralText :: MonadTokenParse m => m ExpandedBalancedText
parseExpandedGeneralText = _parseDelimitedText parseExpandedBalancedText