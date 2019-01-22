{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module HeX.Parse.Inhibited where

import           Control.Monad                  ( guard
                                                , foldM )
import           Safe                           ( lastMay
                                                , initMay
                                                )
import           Data.Char                      ( ord )
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )
import           Data.Functor                   ( ($>) )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )

import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.AST
import           HeX.Parse.Token
import           HeX.Parse.Common

-- Cases where expansion is inhibited:
-- 1.  While deleting tokens during error recovery
-- 2.  While skipping tokens because conditional text is being ignored
-- 3.  While reading macro arguments
-- 4.  While reading a control sequence to be defined by \let, \futurelet, \def, \gdef, \edef, \xdef, \chardef, \mathchardef, \countdef, \dimendef, \skipdef, \muskipdef, \toksdef, \read or \font
-- 5.  While reading argument tokens for \expandafter, \noexpand, \string, \meaning, \let, \futurelet, \ifx, \show, \afterassignment or \aftergroup
-- 6.  While absorbing the parameter text of a \def, \gdef, \edef or \xdef
-- 7.  While absorbing the replacement text of a \def, \gdef or \read; or the text of a token variable like \everypar or \toks0; or the token list for \uppercase or \lowercase or \write. (The token list for \write will be expanded later, when it is actually output to a file.)
-- 8.  While reading the preamble of an alignment, except after a token for the primitive command \span or when reading the ⟨glue⟩ after \tabskip
-- 9.  Just after a <$,3> token that begins math mode, to see if another $3 follows
-- 10.  Just after a <‘,12> token that begins an alphabetic constant

-- Case 3, macro arguments.

newtype MacroArgument = MacroArgument [Lex.Token]
    deriving (Show, Eq)

nrExpressions :: (a -> Ordering) -> [a] -> Maybe (Int, Int)
nrExpressions f = foldM next (0, 0)
  where
    next v@(depth, nrExprs) (f -> cmp) =
        case cmp of
            EQ -> Just v
            GT -> Just (succ depth, nrExprs)
            LT
                | depth < 1  -> Nothing
                | depth == 1 -> Just (pred depth, succ nrExprs)
                | otherwise  -> Just (pred depth, nrExprs)

hasValidGrouping :: (a -> Ordering) -> [a] -> Bool
hasValidGrouping f xs =
    case nrExpressions f xs of
        Just (0, _) -> True
        _ -> False

splitLastMay :: [a] -> Maybe ([a], a)
splitLastMay xs =
    do
    z <- lastMay xs
    ini <- initMay xs
    pure (ini, z)

unsafeParseMacroArgs :: (P.Stream s, P.Token s ~ PrimitiveToken) => MacroContents -> SimpParser s (Map.Map Digit MacroArgument)
unsafeParseMacroArgs MacroContents{preParamTokens=pre, parameters=params} =
    do
    skipSatisfiedLexChunk pre
    parseArgs params
  where
    parseArgs
        :: (P.Stream s, P.Token s ~ PrimitiveToken)
        => Map.Map Digit [Lex.Token]
        -> SimpParser s (Map.Map Digit MacroArgument)
    parseArgs ps =
        case Map.minViewWithKey ps of
            -- If there are no parameters, expect no arguments.
            Nothing               -> pure Map.empty
            Just ((dig, p), rest) ->
                do
                argRaw <- case p of
                  [] -> parseUndelimitedArgs
                  delims -> parseDelimitedArgs [] delims
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
    parseUndelimitedArgs
        :: (P.Stream s, P.Token s ~ PrimitiveToken)
        => SimpParser s [Lex.Token]
    parseUndelimitedArgs =
        do
        -- Skip blank tokens (assumed to mean spaces).
        skipManySatisfied (primTokHasCategory Lex.Space)
        unsafeAnySingleLex >>= \case
            t@(Lex.CharCatToken Lex.CharCat{cat = Lex.BeginGroup}) ->
                do
                (BalancedText ts) <- unsafeParseBalancedText Include
                pure $ t:ts
            t ->
                pure [t]

    -- Get the shortest, possibly empty, properly nested sequence of tokens,
    -- followed by the delimiter tokens. In the delimiter, category codes,
    -- character codes and control sequence names must match.
    parseDelimitedArgs
        :: (P.Stream s, P.Token s ~ PrimitiveToken)
        => [Lex.Token]
        -> [Lex.Token]
        -> SimpParser s [Lex.Token]
    parseDelimitedArgs ts delims = do
        -- Parse tokens until we see the delimiter tokens, then add what we grab
        -- to our accumulating argument.
        arg <- (ts ++) <$> P.manyTill unsafeAnySingleLex (skipSatisfiedLexChunk delims)
        if hasValidGrouping tokToChange arg
            -- If the argument has valid grouping, then we are done.
            then pure arg
            -- Otherwise, add the 'red herring' delimiters we just parsed and
            -- continue.
            else parseDelimitedArgs (arg ++ delims) delims

    -- Check if an argument has an outer '{}' pair that should be stripped, and
    -- do this if so.
    -- If we got an empty argument, can consider this to 'strip' to itself.
    getStripped [] = Nothing
    getStripped (a:xs) =
        do
        -- First token must be a '{'.
        guard $ lexTokHasCategory Lex.BeginGroup a
        -- Must have at least two tokens. If so, get the last token, 'z', and the
        -- tokens that sit before it, i.e. the stripped argument.
        (inner, z) <- splitLastMay xs
        -- The last token must be a '}'.
        guard $ lexTokHasCategory Lex.EndGroup z
        -- The stripped argument must have valid grouping.
        guard $ hasValidGrouping tokToChange inner
        -- Return the stripped argument.
        pure inner

-- Case 4, for things like 'macroName' in '\def\macroName'.

unsafeParseCSName :: (P.Stream s, P.Token s ~ PrimitiveToken) => SimpParser s Lex.ControlSequenceLike
unsafeParseCSName = handleLex tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken Lex.CharCat{cat = Lex.Active, char = c}) =
        Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) =
        Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ =
        Nothing

-- Case 5, arbitrary tokens such as for \let\foo=<token>.

unsafeAnySingleLex
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => SimpParser s Lex.Token
unsafeAnySingleLex = satisfyThen tokToLex

-- Case 6, macro parameter text.

parseParamDelims
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => SimpParser s [Lex.Token]
parseParamDelims = manySatisfiedThen (\t -> tokToDelimTok t)
  where
    tokToDelimTok (UnexpandedTok lt)
        | lexTokHasCategory Lex.Parameter lt  = Nothing
        | lexTokHasCategory Lex.BeginGroup lt = Nothing
        | otherwise                           = Just lt
    tokToDelimTok _                           = Nothing

maybeParseParametersFrom
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => Digit
    -> SimpParser s MacroParameters
maybeParseParametersFrom dig = parseEndOfParams <|> parseParametersFrom
  where
    -- Parse the left-brace that indicates the end of parameters.
    parseEndOfParams = skipSatisfied (primTokHasCategory Lex.BeginGroup) $> Map.empty

    -- Parse a present parameter, then the remaining parameters, if present.
    parseParametersFrom =
        do
        -- Parse, for example, '#3'.
        skipSatisfied $ primTokHasCategory Lex.Parameter
        skipSatisfied $ liftLexPred matchesDigit
        -- Parse delimiter tokens after the parameter number, if present.
        thisParam <- parseParamDelims
        -- Return this parameter, plus any remaining parameters.
        Map.insert dig thisParam <$> case dig of
            -- If we are parsing parameter nine, there can't be any more, so we
            -- only expect to end the parameters.
            Nine -> parseEndOfParams
            -- Otherwise, we can either end the parameters, or have some more,
            -- starting from the successor of this digit.
            _    -> maybeParseParametersFrom (succ dig)

    matchesDigit (Lex.CharCatToken Lex.CharCat {char=chr, cat=cat})
        = (cat `elem` [Lex.Letter, Lex.Other]) && (chr == digitToChar dig)
    matchesDigit _
        = False

unsafeParseParamText
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => SimpParser s ([Lex.Token], MacroParameters)
unsafeParseParamText =
    do
    -- Pre-parameter text tokens.
    preParamToks <- parseParamDelims
    -- Parameters, if present.
    params <- maybeParseParametersFrom minBound
    pure (preParamToks, params)

-- Case 7, general lists of token.

-- How to handle a terminal item.
data TerminusPolicy = Include | Discard
    deriving (Show, Eq)

parseNestedExpr
  :: P.Stream s
  -- The current depth of the expression.
  => Int
  -- Parse a token and evaluate its effect on the stack.
  -- GT means 'push', LT means 'pop', EQ means 'no change')
  -- Example: '(' -> GT, ')' -> LT, anything else -> EQ.
  -> P.Parsec e s (a, Ordering)
  -> TerminusPolicy
  -- Nested expression with valid grouping.
  -> P.Parsec e s [a]
parseNestedExpr 0 _ _         = pure []
parseNestedExpr n parseNext policy =
    do
    (x, change) <- parseNext
    -- Get next stack depth.
    let nextN = case change of
            LT -> pred n
            GT -> succ n
            EQ -> n
    case nextN of
        -- When we reach zero depth, we are done.
        -- Catch it early, rather than recursing, to avoid returning the final ')',
        -- because we don't want it.
        0 -> pure $ case policy of
            Include -> [x]
            Discard -> []
        -- Otherwise, append our result and continue.
        _ -> (x :) <$> parseNestedExpr nextN parseNext policy

-- Part of case 7, \uppercase's body and such.

tokToChange :: Lex.Token -> Ordering
tokToChange t
    | lexTokHasCategory Lex.BeginGroup t = GT
    | lexTokHasCategory Lex.EndGroup t   = LT
    | otherwise                          = EQ

-- This assumes we just parsed the '{' that starts the balanced text.
unsafeParseBalancedText
    :: (P.Stream s, P.Token s ~ PrimitiveToken)
    => TerminusPolicy
    -> SimpParser s BalancedText
unsafeParseBalancedText policy = BalancedText <$> parseNestedExpr 1 parseNext policy
  where
    parseNext = handleLex $ \t -> Just (t, tokToChange t)

-- Part of case 7, macro replacement text.

-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.

-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like unsafeParseBalancedText, but extracts argument calls.
unsafeParseMacroText :: (P.Stream s, P.Token s ~ PrimitiveToken) => SimpParser s MacroText
unsafeParseMacroText = MacroText <$> parseNestedExpr 1 parseNext Discard
  where
    parseNext =
        unsafeAnySingleLex >>= \case
            -- If we see a '#', parse the parameter number and return a token
            -- representing the call.
            (Lex.CharCatToken Lex.CharCat{cat = Lex.Parameter}) -> do
                paramDig <- handleLex handleParamNr
                pure (MacroTextParamToken paramDig, EQ)
            -- Otherwise, just return the ordinary lex token.
            t ->
                pure (MacroTextLexToken t, tokToChange t)

    -- We are only happy if the '#' is followed by a decimal digit.
    handleParamNr (Lex.CharCatToken Lex.CharCat{char=chr, cat=cat})
        | cat `elem` [Lex.Letter, Lex.Other] = charToDigit chr
        | otherwise = Nothing
    handleParamNr _ = Nothing

-- Case 10, character constant like "`c".

unsafeParseCharLike :: (P.Stream s, P.Token s ~ PrimitiveToken) => SimpParser s Int
unsafeParseCharLike = ord <$> handleLex tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken Lex.CharCat{char = c}) = Just c
    tokToCharLike (Lex.ControlSequenceToken (Lex.ControlSequence [c])) = Just c
    tokToCharLike _ = Nothing
