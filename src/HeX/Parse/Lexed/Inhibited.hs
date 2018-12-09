{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module HeX.Parse.Lexed.Inhibited where

import           Safe                           ( lastMay
                                                , initMay
                                                )
import qualified Data.Char                     as C
import qualified Data.Map.Strict               as Map
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )
import           Control.Monad                  ( guard )
import           Data.Functor                   ( ($>) )
import           Data.Foldable                  ( foldl' )
import           Data.Maybe                     ( fromMaybe )

import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Lexed.Stream

isCategory :: Lex.LexCatCode -> Lex.Token -> Bool
isCategory a (Lex.CharCatToken Lex.CharCat{cat = b}) = a == b
isCategory _ _ = False

isLetterOrOther :: Lex.LexCatCode -> Bool
isLetterOrOther Lex.Letter = True
isLetterOrOther Lex.Other = True
isLetterOrOther _ = False

isBeginGroup, isEndGroup :: Lex.Token -> Bool
isBeginGroup = isCategory Lex.BeginGroup
isEndGroup = isCategory Lex.EndGroup

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
parseNestedExpr n parseNext policy = do
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

-- Case 3.

newtype MacroArgument = MacroArgument [Lex.Token]
  deriving (Show, Eq)

nrExpressions :: (a -> Ordering) -> [a] -> Maybe (Int, Int)
nrExpressions f = foldl' next (Just (0, 0))
  where
    next Nothing _ = Nothing
    next v (f -> EQ) = v
    next (Just (depth, nrExprs)) (f -> GT) = Just (succ depth, nrExprs)
    next (Just (depth, nrExprs)) (f -> LT)
      | depth < 1 = Nothing
      | depth == 1 = Just (pred depth, succ nrExprs)
      | otherwise = Just (pred depth, nrExprs)

hasValidGrouping :: (a -> Ordering) -> [a] -> Bool
hasValidGrouping f xs = case nrExpressions f xs of
  Just (0, _) -> True
  _ -> False

splitLastMay :: [a] -> Maybe ([a], a)
splitLastMay xs = do
  z <- lastMay xs
  ini <- initMay xs
  pure (ini, z)

parseMacroArgs :: MacroContents -> SimpLexParser (Map.Map Digit MacroArgument)
parseMacroArgs MacroContents {preParamTokens=pre, parameters=params} = do
  skipSatisfiedChunk pre
  parseArgs params
  where
    parseArgs :: Map.Map Digit [Lex.Token] -> SimpLexParser (Map.Map Digit MacroArgument)
    parseArgs ps = case Map.minViewWithKey ps of
      -- If there are no parameters, expect no arguments.
      Nothing -> pure Map.empty
      Just ((dig, p), rest) -> do
        argRaw <- case p of
          [] -> parseUndelimitedArgs
          delims -> parseDelimitedArgs [] delims
        -- If appropriate, strip the argument; otherwise use the unstripped
        -- version.
        let arg = fromMaybe argRaw (getStripped argRaw)
        Map.insert dig (MacroArgument arg) <$> parseArgs rest

    -- If the parameter is undelimited, the argument is the next non-blank
    -- token, unless that token is ‘{’, when the argument will be the entire
    -- following {...} group.
    parseUndelimitedArgs :: SimpLexParser [Lex.Token]
    parseUndelimitedArgs = do
      -- Skip blank tokens (assumed to mean spaces).
      skipManySatisfied (isCategory Lex.Space)
      getToken >>= \case
        t@(Lex.CharCatToken Lex.CharCat {cat = Lex.BeginGroup}) -> do
          (BalancedText ts) <- parseBalancedText Include
          pure (t:ts)
        t ->
          pure [t]

    -- Get the shortest, possibly empty, properly nested sequence of tokens,
    -- followed by the delimiter tokens. In the delimiter, category codes,
    -- character codes and control sequence names must match.
    parseDelimitedArgs :: [Lex.Token] -> [Lex.Token] -> SimpLexParser [Lex.Token]
    parseDelimitedArgs ts delims = do
      -- Parse tokens until we see the delimiter tokens, then add what we grab
      -- to our accumulating argument.
      arg <- (ts ++) <$> P.manyTill getToken (skipSatisfiedChunk delims)
      if hasValidGrouping tokToChange arg
        -- If the argument has valid grouping, then we are done.
        then pure arg
        -- Otherwise, add the 'red herring' delimiters we just parsed and
        -- continue.
        else parseDelimitedArgs (arg ++ delims) delims

    -- Check if an argument has an outer '{}' pair that should be stripped, and
    -- do this if so.
    -- If we got an empty argument, can consider this as 'stripped' to itself.
    getStripped [] = pure []
    getStripped (a:xs) = do
      -- First token must be a '{'.
      guard $ isBeginGroup a
      -- Must have at least two tokens. If so, get the last token, 'z', and the
      -- tokens that sit before it, i.e. the stripped argument.
      (inner, z) <- splitLastMay xs
      -- The last token must be a '}'.
      guard $ isEndGroup z
      -- The stripped argument must have valid grouping.
      guard $ hasValidGrouping tokToChange inner
      -- Return the stripped argument.
      pure inner

-- Part of case 7, \uppercase's body and such.

newtype BalancedText = BalancedText [Lex.Token]
  deriving (Show, Eq)

tokToChange :: Lex.Token -> Ordering
tokToChange t
  | isBeginGroup t = GT
  | isEndGroup t = LT
  | otherwise = EQ

-- This assumes we just parsed the '{' that starts the balanced text.
parseBalancedText :: TerminusPolicy -> SimpLexParser BalancedText
parseBalancedText policy = BalancedText <$> parseNestedExpr 1 parseNext policy
  where
    parseNext = getToken >>= (\x -> pure (x, tokToChange x))

-- Part of case 7, macro replacement text.

-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.

data Digit
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord, Bounded, Enum, Show)

digitToChar :: Digit -> Char
digitToChar One   = '1'
digitToChar Two   = '2'
digitToChar Three = '3'
digitToChar Four  = '4'
digitToChar Five  = '5'
digitToChar Six   = '6'
digitToChar Seven = '7'
digitToChar Eight = '8'
digitToChar Nine  = '9'

charToDigit :: Char -> Maybe Digit
charToDigit '1' = Just One
charToDigit '2' = Just Two
charToDigit '3' = Just Three
charToDigit '4' = Just Four
charToDigit '5' = Just Five
charToDigit '6' = Just Six
charToDigit '7' = Just Seven
charToDigit '8' = Just Eight
charToDigit '9' = Just Nine
charToDigit _   = Nothing

-- We use a map to restrict our parameter keys' domain to [1..9].
type MacroParameters = Map.Map Digit [Lex.Token]

-- A token in a macro template.
-- TODO: Technically, we could narrow the domain of a MacroTextLexToken,
-- because we should know that we won't have a 'Parameter'-category token.
data MacroTextToken
  -- A 'normal' token.
  = MacroTextLexToken Lex.Token
  -- A token to be substituted by a macro argument.
  | MacroTextParamToken Digit
  deriving (Eq, Show)

-- A macro template.
newtype MacroText = MacroText [MacroTextToken]
  deriving (Show, Eq)

data MacroContents
  = MacroContents {
      -- Tokens to expect before the first argument.
      preParamTokens :: [Lex.Token]
      , parameters :: MacroParameters
      , replacementTokens :: MacroText }
  deriving (Show, Eq)

-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like parseBalancedText, but extracts argument calls.
parseMacroText :: SimpLexParser MacroText
parseMacroText = MacroText <$> parseNestedExpr 1 parseNext Discard
  where
    parseNext :: SimpLexParser (MacroTextToken, Ordering)
    parseNext = getToken >>= \case
      -- If we see a '#', parse the parameter number and return a token
      -- representing the call.
      (Lex.CharCatToken Lex.CharCat {cat = Lex.Parameter}) -> do
        paramDig <- satisfyThen handleParamNr
        pure (MacroTextParamToken paramDig, EQ)
      -- Otherwise, just return the ordinary lex token.
      t ->
        pure (MacroTextLexToken t, tokToChange t)

    -- We are only happy if the '#' is followed by a decimal digit.
    handleParamNr (Lex.CharCatToken Lex.CharCat {char=chr, cat=cat})
      | isLetterOrOther cat = charToDigit chr
      | otherwise = Nothing
    handleParamNr _ = Nothing

-- Case 10, character constant like "`c".
parseCharLike :: SimpLexParser Integer
parseCharLike = fromIntegral . C.ord <$> satisfyThen tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken Lex.CharCat {char = c}) =
      Just c
    tokToCharLike (Lex.ControlSequenceToken (Lex.ControlSequence [c])) =
      Just c
    tokToCharLike _ = Nothing

-- Case 4, for things like 'macroName' in '\def\macroName'.
parseCSName :: SimpLexParser Lex.ControlSequenceLike
parseCSName = satisfyThen tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken Lex.CharCat {cat = Lex.Active, char = c}) =
      Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) = Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

-- Case 6.
parseParamDelims :: SimpLexParser [Lex.Token]
parseParamDelims = manySatisfied (not . endsDelim)
  where
    endsDelim t = isCategory Lex.Parameter t || isCategory Lex.BeginGroup t

maybeParseParametersFrom :: Digit -> SimpLexParser MacroParameters
maybeParseParametersFrom dig = parseEndOfParams <|> parseParametersFrom
  where
    -- Parse the left-brace that indicates the end of parameters.
    parseEndOfParams = skipSatisfied (isCategory Lex.BeginGroup) $> Map.empty

    -- Parse a present parameter, then the remaining parameters, if present.
    parseParametersFrom = do
      -- Parse, for example, '#3'.
      skipSatisfied $ isCategory Lex.Parameter
      skipSatisfied matchesDigit
      -- Parse delimiter tokens after the parameter number, if present.
      thisParam <- parseParamDelims
      -- Return this parameter, plus any remaining parameters.

      Map.insert dig thisParam <$> case dig of
        -- If we are parsing parameter nine, there can't be any more, so we
        -- only expect to end the parameters.
        Nine -> parseEndOfParams
        -- Otherwise, we can either end the parameters, or have some more,
        -- starting from the successor of this digit.
        _ -> maybeParseParametersFrom (succ dig)

    matchesDigit (Lex.CharCatToken Lex.CharCat {char=chr, cat=cat})
      | isLetterOrOther cat = chr == digitToChar dig
      | otherwise = False
    matchesDigit _ = False

parseParamText :: SimpLexParser ([Lex.Token], MacroParameters)
parseParamText = do
  -- Pre-parameter text tokens.
  preParamToks <- parseParamDelims
  -- Parameters, if present.
  params <- maybeParseParametersFrom minBound
  pure (preParamToks, params)
