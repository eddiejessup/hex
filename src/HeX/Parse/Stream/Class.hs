{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module HeX.Parse.Stream.Class where

import           HeXlude                   hiding (many)

import           Control.Monad             (foldM, guard)
import qualified Control.Monad.Combinators as PC
import           Control.Monad.State.Lazy  (MonadState, StateT, modify,
                                            runStateT)
import           Data.Char                 (ord, toLower, toUpper)
import qualified Data.Foldable             as Fold
import           Data.Functor              (($>))
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Path                 as D.Path

import           HeX.Categorise            (CharCode)
import           HeX.Config                (Config, ConfigError)
import           HeX.Evaluate
import           HeX.Lex                   (CharCat (..))
import qualified HeX.Lex                   as Lex
import           HeX.Parse.Parser
import           HeX.Parse.Resolve
import           HeX.Parse.Token

class TeXStream s where

    setExpansion :: ExpansionMode -> s -> s

    getConfig :: s -> Config

    setConfig :: Config -> s -> s

    insertLexToken :: s -> Lex.Token -> s

    getConditionBodyState :: s -> Maybe ConditionBodyState

    takeToken
        :: TeXStreamM e m
        => s
        -> m (s, PrimitiveToken)

data EndOfInputError = EndOfInputError
    deriving (Show)

type TeXStreamE =
   '[ StreamTakeError
    , EndOfInputError
    , EvaluationError
    , ConfigError
    , D.Path.PathError
    ]

type TeXStreamM e m = (MonadErrorAnyOf e m TeXStreamE, MonadIO m)

type TeXParser s e m a = (TeXStream s, TeXStreamM e m) => SParser s m a

insertLexTokens :: TeXStream s => s -> [Lex.Token] -> s
insertLexTokens s ts = Fold.foldl' insertLexToken s $ reverse ts

satisfyThen :: (PrimitiveToken -> Maybe a) -> TeXParser s e m a
satisfyThen test = SParser go
  where
    go stream =
        do
        (newStream, tok) <- takeToken stream
        case test tok of
          Nothing ->
            throwM $ ParseError $ "Not satisfied with token: " <> show tok
          Just x ->
            pure (newStream, x)

satisfy :: (PrimitiveToken -> Bool) -> TeXParser s e m PrimitiveToken
satisfy f = satisfyThen (predToMaybe f)

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True x  = Just x
boolToMaybe False _ = Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe f x = boolToMaybe (f x) x

anySingle :: TeXParser s e m PrimitiveToken
anySingle = satisfy (const True)

type MatchToken s = PrimitiveToken -> Bool

manySatisfied :: MatchToken s -> TeXParser s e m [PrimitiveToken]
manySatisfied testTok = PC.many $ satisfy testTok

manySatisfiedThen :: (PrimitiveToken -> Maybe a) -> TeXParser s e m [a]
manySatisfiedThen f = PC.many $ satisfyThen f

-- Skipping.
skipSatisfied :: MatchToken s -> TeXParser s e m ()
skipSatisfied f = satisfyThen $ \x -> if f x
          then Just ()
          else Nothing

skipSatisfiedEquals :: Eq PrimitiveToken => PrimitiveToken -> TeXParser s e m ()
skipSatisfiedEquals t = skipSatisfied (== t)

skipOptional :: TeXParser s e m a -> TeXParser s e m ()
skipOptional p = optional p $> ()

skipOneOptionalSatisfied :: MatchToken s -> TeXParser s e m ()
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: MatchToken s -> TeXParser s e m ()
skipManySatisfied = PC.skipMany . skipSatisfied

skipSatisfiedChunk :: Eq PrimitiveToken => [PrimitiveToken] -> TeXParser s e m ()
skipSatisfiedChunk = foldr (skipSatisfiedEquals >>> (>>)) (pure ())

inhibitExpansion, enableExpansion :: TeXStream s => s -> s
inhibitExpansion = setExpansion NotExpanding
enableExpansion = setExpansion Expanding

getInput :: Applicative m => SParser s m s
getInput = SParser $ \s -> pure (s, s)

parseInhibited :: TeXParser s e m a -> TeXParser s e m a
parseInhibited (SParser baseParser) = SParser go
  where
    go stream =
        do
        (postStream, v) <- baseParser (inhibitExpansion stream)
        pure (enableExpansion postStream, v)

runConfState :: (TeXStream s, MonadState s m)
             => StateT Config m a
             -> m a
runConfState f = do
    conf <- gets getConfig
    (v, conf') <- runStateT f conf
    modify $ setConfig conf'
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
newtype MacroArgument = MacroArgument [Lex.Token]
    deriving ( Show, Eq )

nrExpressions :: (a -> Ordering) -> [a] -> Maybe (Int, Int)
nrExpressions f = foldM next (0, 0)
  where
    next v@(dpth, nrExprs) x = case f x of
        EQ -> Just v
        GT -> Just (succ dpth, nrExprs)
        LT
            | dpth < 1 -> Nothing
            | dpth == 1 -> Just (pred dpth, succ nrExprs)
            | otherwise -> Just (pred dpth, nrExprs)

hasValidGrouping :: (a -> Ordering) -> [a] -> Bool
hasValidGrouping f xs = case nrExpressions f xs of
    Just (0, _) -> True
    _           -> False

splitLastMay :: [a] -> Maybe ([a], a)
splitLastMay xs = do
    z <- lastMay xs
    ini <- initMay xs
    pure (ini, z)

unsafeParseMacroArgs :: MacroContents -> TeXParser s e m (Map.Map Digit MacroArgument)
unsafeParseMacroArgs MacroContents{preParamTokens = pre, parameters = params} =
    do
        skipBalancedText pre
        parseArgs params
  where
    parseArgs :: MacroParameters -> TeXParser s e m (Map.Map Digit MacroArgument)
    parseArgs ps = case Map.minViewWithKey ps of
        -- If there are no parameters, expect no arguments.
        Nothing -> pure Map.empty
        Just ((dig, p), rest) -> do
            argRaw <- case p of
                BalancedText []     -> parseUndelimitedArgs
                BalancedText delims -> parseDelimitedArgs [] delims
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
    parseUndelimitedArgs :: TeXParser s e m [Lex.Token]
    parseUndelimitedArgs = do
        -- Skip blank tokens (assumed to mean spaces).
        skipManySatisfied (primTokHasCategory Lex.Space)
        unsafeAnySingleLex >>= \case
                t@(Lex.CharCatToken CharCat{cat = Lex.BeginGroup}) -> do
                    (BalancedText ts) <- unsafeParseBalancedText Include
                    pure $ t : ts
                t -> pure [ t ]

    -- Get the shortest, possibly empty, properly nested sequence of tokens,
    -- followed by the delimiter tokens. In the delimiter, category codes,
    -- character codes and control sequence names must match.
    parseDelimitedArgs :: [Lex.Token] -> [Lex.Token] -> TeXParser s e m [Lex.Token]
    parseDelimitedArgs ts delims = do
        -- Parse tokens until we see the delimiter tokens, then add what we grab
        -- to our accumulating argument.
        arg <- (ts <>) <$> PC.manyTill unsafeAnySingleLex (skipSatisfiedLexChunk delims)
        if hasValidGrouping tokToChange arg
            then
                -- If the argument has valid grouping, then we are done.
                pure arg
            else
                -- Otherwise, add the 'red herring' delimiters we just parsed and
                -- continue.
                parseDelimitedArgs (arg <> delims) delims

    -- Check if an argument has an outer '{}' pair that should be stripped, and
    -- do this if so.
    -- If we got an empty argument, can consider this to 'strip' to itself.
    getStripped [] = Nothing
    getStripped (a : xs) = do
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
unsafeParseCSName :: TeXParser s e m Lex.ControlSequenceLike
unsafeParseCSName = handleLex tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken CharCat{cat = Lex.Active, char = c}) =
        Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) =
        Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

-- Case 5, arbitrary tokens such as for \let\foo=<token>.
unsafeAnySingleLex :: TeXParser s e m Lex.Token
unsafeAnySingleLex = satisfyThen tokToLex

-- Case 6, macro parameter text.
-- Trivially balanced, because no braces are allowed at all.
parseParamDelims :: TeXParser s e m BalancedText
parseParamDelims = BalancedText <$> manySatisfiedThen tokToDelimTok
  where
    tokToDelimTok (UnexpandedTok lt)
        | lexTokHasCategory Lex.Parameter lt = Nothing
        | lexTokHasCategory Lex.BeginGroup lt = Nothing
        | lexTokHasCategory Lex.EndGroup lt = Nothing
        | otherwise = Just lt
    tokToDelimTok _ = Nothing

maybeParseParametersFrom :: Digit -> TeXParser s e m MacroParameters
maybeParseParametersFrom dig = parseEndOfParams <|> parseParametersFrom
    -- Parse the left-brace that indicates the end of parameters.

      where
        parseEndOfParams = skipSatisfied (primTokHasCategory Lex.BeginGroup)
            $> Map.empty

        -- Parse a present parameter, then the remaining parameters, if present.
        parseParametersFrom = do
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

        matchesDigit (Lex.CharCatToken CharCat{char = c, cat = cat}) =
            (cat `elem` [ Lex.Letter, Lex.Other ]) && (c == digitToChar dig)
        matchesDigit _ = False

unsafeParseParamText :: TeXParser s e m (BalancedText, MacroParameters)
unsafeParseParamText = do
    -- Pre-parameter text tokens.
    preParamToks <- parseParamDelims
    -- Parameters, if present.
    params <- maybeParseParametersFrom minBound
    pure (preParamToks, params)

-- Case 7, general lists of token.
-- How to handle a terminal item.
data TerminusPolicy = Include | Discard
    deriving ( Show, Eq )

-- Nested expression with valid grouping.
parseNestedExpr :: Int -> TeXParser s e m (a, Ordering) -> TerminusPolicy -> TeXParser s e m [a]
parseNestedExpr 0 _ _ = pure []
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
        0 -> pure $
            case policy of
                Include -> [ x ]
                Discard -> []
        -- Otherwise, append our result and continue.
        _ -> (x :) <$> parseNestedExpr nextN parseNext policy

-- Part of case 7, \uppercase's body and such.
tokToChange :: Lex.Token -> Ordering
tokToChange t
    | lexTokHasCategory Lex.BeginGroup t = GT
    | lexTokHasCategory Lex.EndGroup t = LT
    | otherwise = EQ

-- This assumes we just parsed the '{' that starts the balanced text.
unsafeParseBalancedText :: TerminusPolicy -> TeXParser s e m BalancedText
unsafeParseBalancedText policy =
    BalancedText <$> parseNestedExpr 1 parseNext policy
  where
    parseNext = handleLex $ \t -> Just (t, tokToChange t)

-- Part of case 7, macro replacement text.
-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.
-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like unsafeParseBalancedText, but extracts argument calls.
unsafeParseMacroText :: TeXParser s e m MacroText
unsafeParseMacroText = MacroText <$> parseNestedExpr 1 parseNext Discard
  where
    parseNext = unsafeAnySingleLex
        >>= \case
            -- If we see a '#', parse the parameter number and return a token
            -- representing the call.
            Lex.CharCatToken CharCat{cat = Lex.Parameter} ->
                handleLex handleParamNr <&> (, EQ)
            -- Otherwise, just return the ordinary lex token.
            t -> pure (MacroTextLexToken t, tokToChange t)

    -- We are happy iff the '#' is followed by a decimal digit, or another '#'.
    handleParamNr = \case
        Lex.CharCatToken cc@CharCat{char = c, cat = cat}
            | cat `elem` [ Lex.Letter, Lex.Other ] ->
                MacroTextParamToken <$> charToDigit c
            | cat == Lex.Parameter ->
                pure $ MacroTextLexToken $ Lex.CharCatToken cc{ cat = Lex.Other }
        _ ->
            Nothing


-- Case 10, character constant like "`c".
unsafeParseCharLike :: TeXParser s e m Int
unsafeParseCharLike = ord <$> handleLex tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken CharCat{char = c}) =
        Just c
    tokToCharLike (Lex.ControlSequenceToken (Lex.ControlSequence (FDirected [ c ]))) =
        Just c
    tokToCharLike _ =
        Nothing

-- Interface.
parseBalancedText :: TerminusPolicy -> TeXParser s e m BalancedText
parseBalancedText = parseInhibited . unsafeParseBalancedText

parseMacroArgs :: MacroContents -> TeXParser s e m (Map.Map Digit MacroArgument)
parseMacroArgs = parseInhibited . unsafeParseMacroArgs

parseCharLike :: TeXParser s e m Int
parseCharLike = parseInhibited unsafeParseCharLike

parseCSName :: TeXParser s e m Lex.ControlSequenceLike
parseCSName = parseInhibited unsafeParseCSName

parseParamText :: TeXParser s e m (BalancedText, MacroParameters)
parseParamText = parseInhibited unsafeParseParamText

parseMacroText :: TeXParser s e m MacroText
parseMacroText = parseInhibited unsafeParseMacroText

parseLexToken :: TeXParser s e m Lex.Token
parseLexToken = parseInhibited unsafeAnySingleLex

parseLetArg :: TeXParser s e m Lex.Token
parseLetArg = parseInhibited skipOneOptionalSpace >> parseLexToken

-- Derived related parsers.
skipFiller :: TeXParser s e m ()
skipFiller = skipManySatisfied isFillerItem

parseExpandedBalancedText :: TerminusPolicy -> TeXParser s e m ExpandedBalancedText
parseExpandedBalancedText policy =
    ExpandedBalancedText <$> parseNestedExpr 1 parseNext policy
  where
    parseNext = satisfyThen $ \pt -> Just (pt, primTokToChange pt)

    primTokToChange = \case
        UnexpandedTok lt -> tokToChange lt
        _ -> EQ

_parseDelimitedText
    :: (TerminusPolicy -> SParser s m a)
    -> TeXParser s e m a
_parseDelimitedText parser = do
    skipFiller
    skipLeftBrace
    parser Discard

parseGeneralText :: TeXParser s e m BalancedText
parseGeneralText = _parseDelimitedText parseBalancedText

parseExpandedGeneralText :: TeXParser s e m ExpandedBalancedText
parseExpandedGeneralText = _parseDelimitedText parseExpandedBalancedText





-- Helpers.
ccHasCategory :: Lex.LexCatCode -> CharCat -> Bool
ccHasCategory a CharCat{cat = b} = a == b

lexTokHasCategory :: Lex.LexCatCode -> Lex.Token -> Bool
lexTokHasCategory a (Lex.CharCatToken cc) = ccHasCategory a cc
lexTokHasCategory _ _                     = False

primTokHasCategory :: Lex.LexCatCode -> PrimitiveToken -> Bool
primTokHasCategory a (UnexpandedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _                  = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory Lex.Space

-- Match particular tokens.
isFillerItem :: PrimitiveToken -> Bool
isFillerItem RelaxTok = True
isFillerItem t        = isSpace t

matchOtherToken :: CharCode -> PrimitiveToken -> Bool
matchOtherToken c2
                (UnexpandedTok (Lex.CharCatToken CharCat{ cat = Lex.Other
                                                      , char = c1
                                                      })) = c1 == c2
matchOtherToken _ _ = False

isEquals :: PrimitiveToken -> Bool
isEquals = matchOtherToken '='

matchNonActiveCharacterUncased :: Char -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased
    a
    (UnexpandedTok (Lex.CharCatToken CharCat{ char = c , cat = cat })) =
        (cat /= Lex.Active) && (c `elem` [ toUpper a, toLower a ])
matchNonActiveCharacterUncased _ _ = False

tokToChar :: PrimitiveToken -> Maybe CharCode
tokToChar (UnexpandedTok (Lex.CharCatToken CharCat{char = c})) = Just c
tokToChar _                                                    = Nothing

-- Lexed.
tokToLex :: PrimitiveToken -> Maybe Lex.Token
tokToLex (UnexpandedTok t) = Just t
tokToLex _                 = Nothing

handleLex :: (Lex.Token -> Maybe a) -> TeXParser s e m a
handleLex f = satisfyThen $ tokToLex >=> f

skipSatisfiedEqualsLex :: Lex.Token -> TeXParser s e m ()
skipSatisfiedEqualsLex lt = skipSatisfiedEquals (UnexpandedTok lt)

skipSatisfiedLexChunk :: [Lex.Token] -> TeXParser s e m ()
skipSatisfiedLexChunk ts = skipSatisfiedChunk (UnexpandedTok <$> ts)

skipBalancedText :: BalancedText -> TeXParser s e m ()
skipBalancedText (BalancedText toks) = skipSatisfiedLexChunk toks

liftLexPred :: (Lex.Token -> Bool) -> PrimitiveToken -> Bool
liftLexPred f (UnexpandedTok lt) = f lt
liftLexPred _ _                  = False

-- Parsers.
skipOneOptionalSpace :: TeXParser s e m ()
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- TODO: Maybe other things can act as left braces.
skipLeftBrace :: TeXParser s e m ()
skipLeftBrace = skipSatisfied $ primTokHasCategory Lex.BeginGroup

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: TeXParser s e m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: TeXParser s e m ()
skipOptionalEquals = do
    skipOptionalSpaces
    skipOneOptionalSatisfied isEquals

skipKeyword :: [CharCode] -> TeXParser s e m ()
skipKeyword s = skipOptionalSpaces
    *> mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: [CharCode] -> TeXParser s e m Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

parseKeywordToValue :: [CharCode] -> b -> TeXParser s e m b
parseKeywordToValue s = (skipKeyword s $>)

parseManyChars :: TeXParser s e m [CharCode]
parseManyChars = PC.many $ satisfyThen tokToChar
