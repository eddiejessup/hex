{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module HeX.Parse.Inhibited where

import           HeXlude          hiding  (many)

import           Control.Monad            (foldM, guard)
import qualified Control.Monad.Combinators as PC
import           Control.Monad.State.Lazy (MonadState, StateT, modify,
                                           runStateT)
import           Data.Char                (ord, toUpper, toLower)
import qualified Data.Foldable            as Fold
import           Data.Functor             (($>))
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)

import           HeX.Categorise           (CharCode)
import           HeX.Config               (Config)
import           HeX.Evaluate
import           HeX.Lex                  (CharCat (..))
import qualified HeX.Lex                  as Lex
import           HeX.Parse.Resolve
import           HeX.Parse.Token

newtype SParser s m a = SParser { runParser :: s -> m (s, a) }

mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (s, v) = (s, f v)

instance Functor m => Functor (SParser s m) where
    fmap f (SParser parse) = SParser g
      where
        g stream = parse stream <&> mapSnd f

instance Monad m => Applicative (SParser s m) where
    pure v = SParser (\s -> pure (s, v))

    (SParser parseFunc) <*> (SParser parseArg) = SParser g
      where
        g stream =
            do
            (postFuncStream, func) <- parseFunc stream
            parseArg postFuncStream <&> mapSnd func

instance MonadError StreamTakeError m => Alternative (SParser s m) where
    empty = SParser $ const (throwError (ErrorWhileTaking "empty"))

    (SParser pA) <|> (SParser pB) = SParser go
      where
        go stream = pA stream `catchError` (\_ -> pB stream)

instance MonadError e m => MonadError e (SParser s m) where
    throwError e = SParser $ const (throwError e)

    -- catchError (SParser parse) handler = SParser go
    --   where
    --     go stream =
    --         do
    --         parse stream

instance Monad m => Monad (SParser s m) where
    -- m a -> (a -> m b) -> m b 
    (SParser parseA) >>= next = SParser g
      where
        g stream =
            do
            (postAStream, a) <- parseA stream
            runParser (next a) postAStream

instance MonadError StreamTakeError m => MonadPlus (SParser s m) where

type MatchToken s = PrimToken s -> Bool








class TeXStream s where
    type PrimToken s

    setExpansion :: ExpansionMode -> s -> s

    getConfig :: s -> Config

    setConfig :: Config -> s -> s

    insertLexToken :: s -> Lex.Token -> s

    getConditionBodyState :: s -> Maybe ConditionBodyState

    takeToken :: s -> TeXStreamM (s, PrimToken s)

type TeXStreamM = ExceptT StreamTakeError IO

data StreamTakeError
    = EndOfStream
    | ErrorWhileTaking Text
    deriving (Show)

type TeXParser s a = TeXStream s => SParser s TeXStreamM a

insertLexTokens :: TeXStream s => s -> [Lex.Token] -> s
insertLexTokens s ts = Fold.foldl' insertLexToken s $ reverse ts

satisfyThen :: (PrimToken s -> Maybe a) -> TeXParser s a
satisfyThen test = SParser go
  where
    go stream =
        do
        (newStream, tok) <- takeToken stream
        case test tok of
          Nothing ->
            throwError $ ErrorWhileTaking "no match"
          Just x ->
            pure (newStream, x)

satisfy :: (PrimToken s -> Bool) -> TeXParser s (PrimToken s)
satisfy f = satisfyThen (predToMaybe f)

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True x = Just x
boolToMaybe False _ = Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe f x = boolToMaybe (f x) x

anySingle :: TeXParser s (PrimToken s)
anySingle = satisfy (const True)

manySatisfied :: MatchToken s -> TeXParser s [PrimToken s]
manySatisfied testTok = PC.many $ satisfy testTok

manySatisfiedThen :: (PrimToken s -> Maybe a) -> TeXParser s [a]
manySatisfiedThen f = PC.many $ satisfyThen f

-- Skipping.
skipSatisfied :: MatchToken s -> TeXParser s ()
skipSatisfied f = satisfyThen $ \x -> if f x
          then Just ()
          else Nothing

skipSatisfiedEquals :: Eq (PrimToken s) => PrimToken s -> TeXParser s ()
skipSatisfiedEquals t = skipSatisfied (== t)

skipOptional :: TeXParser s a -> TeXParser s ()
skipOptional p = optional p $> ()

skipOneOptionalSatisfied :: MatchToken s -> TeXParser s ()
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: MatchToken s -> TeXParser s ()
skipManySatisfied = PC.skipMany . skipSatisfied

skipSatisfiedChunk :: Eq (PrimToken s) => [PrimToken s] -> TeXParser s ()
skipSatisfiedChunk = foldr (skipSatisfiedEquals >>> (>>)) (pure ())

inhibitExpansion, enableExpansion :: TeXStream s => s -> s
inhibitExpansion = setExpansion NotExpanding
enableExpansion = setExpansion Expanding

getInput :: Applicative m => SParser s m s
getInput = SParser $ \s -> pure (s, s)

parseInhibited :: TeXParser s a -> TeXParser s a
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



type TeXPrimStream s = (TeXStream s, PrimToken s ~ PrimitiveToken)

type TeXPrimParser s a = (PrimToken s ~ PrimitiveToken) => TeXParser s a

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

unsafeParseMacroArgs :: MacroContents -> TeXPrimParser s (Map.Map Digit MacroArgument)
unsafeParseMacroArgs MacroContents{preParamTokens = pre, parameters = params} =
    do
        skipBalancedText pre
        parseArgs params
  where
    parseArgs :: MacroParameters -> TeXPrimParser s (Map.Map Digit MacroArgument)
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
    parseUndelimitedArgs :: TeXPrimParser s [Lex.Token]
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
    parseDelimitedArgs :: [Lex.Token] -> [Lex.Token] -> TeXPrimParser s [Lex.Token]
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
unsafeParseCSName :: TeXPrimParser s Lex.ControlSequenceLike
unsafeParseCSName = handleLex tokToCSLike
  where
    tokToCSLike (Lex.CharCatToken CharCat{cat = Lex.Active, char = c}) =
        Just $ Lex.ActiveCharacter c
    tokToCSLike (Lex.ControlSequenceToken cs) =
        Just $ Lex.ControlSequenceProper cs
    tokToCSLike _ = Nothing

-- Case 5, arbitrary tokens such as for \let\foo=<token>.
unsafeAnySingleLex :: TeXPrimParser s Lex.Token
unsafeAnySingleLex = satisfyThen tokToLex

-- Case 6, macro parameter text.
-- Trivially balanced, because no braces are allowed at all.
parseParamDelims :: TeXPrimParser s BalancedText
parseParamDelims = BalancedText <$> manySatisfiedThen tokToDelimTok
  where
    tokToDelimTok (UnexpandedTok lt)
        | lexTokHasCategory Lex.Parameter lt = Nothing
        | lexTokHasCategory Lex.BeginGroup lt = Nothing
        | lexTokHasCategory Lex.EndGroup lt = Nothing
        | otherwise = Just lt
    tokToDelimTok _ = Nothing

maybeParseParametersFrom :: Digit -> TeXPrimParser s MacroParameters
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

unsafeParseParamText :: TeXPrimParser s (BalancedText, MacroParameters)
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
parseNestedExpr :: Int -> TeXParser s (a, Ordering) -> TerminusPolicy -> TeXPrimParser s [a]
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
unsafeParseBalancedText :: TerminusPolicy -> TeXPrimParser s BalancedText
unsafeParseBalancedText policy =
    BalancedText <$> parseNestedExpr 1 parseNext policy
  where
    parseNext = handleLex $ \t -> Just (t, tokToChange t)

-- Part of case 7, macro replacement text.
-- Handled a bit differently to a standard balanced text, because we want to
-- extract parameter references at definition-time.
-- This assumes we just parsed the '{' that starts the macro text.
-- This function is like unsafeParseBalancedText, but extracts argument calls.
unsafeParseMacroText :: TeXPrimParser s MacroText
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
unsafeParseCharLike :: TeXPrimParser s Int
unsafeParseCharLike = ord <$> handleLex tokToCharLike
  where
    tokToCharLike (Lex.CharCatToken CharCat{char = c}) =
        Just c
    tokToCharLike (Lex.ControlSequenceToken (Lex.ControlSequence (FDirected [ c ]))) =
        Just c
    tokToCharLike _ =
        Nothing

-- Interface.
parseBalancedText :: TerminusPolicy -> TeXPrimParser s BalancedText
parseBalancedText = parseInhibited . unsafeParseBalancedText

parseMacroArgs :: MacroContents -> TeXPrimParser s (Map.Map Digit MacroArgument)
parseMacroArgs = parseInhibited . unsafeParseMacroArgs

parseCharLike :: TeXPrimParser s Int
parseCharLike = parseInhibited unsafeParseCharLike

parseCSName :: TeXPrimParser s Lex.ControlSequenceLike
parseCSName = parseInhibited unsafeParseCSName

parseParamText :: TeXPrimParser s (BalancedText, MacroParameters)
parseParamText = parseInhibited unsafeParseParamText

parseMacroText :: TeXPrimParser s MacroText
parseMacroText = parseInhibited unsafeParseMacroText

parseLexToken :: TeXPrimParser s Lex.Token
parseLexToken = parseInhibited unsafeAnySingleLex

parseLetArg :: TeXPrimParser s Lex.Token
parseLetArg = parseInhibited skipOneOptionalSpace >> parseLexToken

-- Derived related parsers.
skipFiller :: TeXPrimParser s ()
skipFiller = skipManySatisfied isFillerItem

parseExpandedBalancedText :: TerminusPolicy -> TeXPrimParser s ExpandedBalancedText
parseExpandedBalancedText policy =
    ExpandedBalancedText <$> parseNestedExpr 1 parseNext policy
  where
    parseNext = satisfyThen $ \pt -> Just (pt, primTokToChange pt)

    primTokToChange = \case
        UnexpandedTok lt -> tokToChange lt
        _ -> EQ

_parseDelimitedText :: (TerminusPolicy -> SParser s TeXStreamM a) -> TeXPrimParser s a
_parseDelimitedText parser = do
    skipFiller
    skipLeftBrace
    parser Discard

parseGeneralText :: TeXPrimParser s BalancedText
parseGeneralText = _parseDelimitedText parseBalancedText

parseExpandedGeneralText :: TeXPrimParser s ExpandedBalancedText
parseExpandedGeneralText = _parseDelimitedText parseExpandedBalancedText





-- Helpers.
ccHasCategory :: Lex.LexCatCode -> CharCat -> Bool
ccHasCategory a CharCat{cat = b} = a == b

lexTokHasCategory :: Lex.LexCatCode -> Lex.Token -> Bool
lexTokHasCategory a (Lex.CharCatToken cc) = ccHasCategory a cc
lexTokHasCategory _ _ = False

primTokHasCategory :: Lex.LexCatCode -> PrimitiveToken -> Bool
primTokHasCategory a (UnexpandedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory Lex.Space

-- Match particular tokens.
isFillerItem :: PrimitiveToken -> Bool
isFillerItem RelaxTok = True
isFillerItem t = isSpace t

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
tokToChar _ = Nothing

-- Lexed.
tokToLex :: PrimitiveToken -> Maybe Lex.Token
tokToLex (UnexpandedTok t) = Just t
tokToLex _ = Nothing

handleLex :: (Lex.Token -> Maybe a) -> TeXPrimParser s a
handleLex f = satisfyThen $ tokToLex >=> f

skipSatisfiedEqualsLex :: Lex.Token -> TeXPrimParser s ()
skipSatisfiedEqualsLex lt = skipSatisfiedEquals (UnexpandedTok lt)

skipSatisfiedLexChunk :: [Lex.Token] -> TeXPrimParser s ()
skipSatisfiedLexChunk ts = skipSatisfiedChunk (UnexpandedTok <$> ts)

skipBalancedText :: BalancedText -> TeXPrimParser s ()
skipBalancedText (BalancedText toks) = skipSatisfiedLexChunk toks

liftLexPred :: (Lex.Token -> Bool) -> PrimitiveToken -> Bool
liftLexPred f (UnexpandedTok lt) = f lt
liftLexPred _ _ = False

-- Parsers.
skipOneOptionalSpace :: TeXPrimParser s ()
skipOneOptionalSpace = skipOneOptionalSatisfied isSpace

-- TODO: Maybe other things can act as left braces.
skipLeftBrace :: TeXPrimParser s ()
skipLeftBrace = skipSatisfied $ primTokHasCategory Lex.BeginGroup

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: TeXPrimParser s ()
skipOptionalSpaces = skipManySatisfied isSpace

skipOptionalEquals :: TeXPrimParser s ()
skipOptionalEquals = do
    skipOptionalSpaces
    skipOneOptionalSatisfied isEquals

skipKeyword :: [CharCode] -> TeXPrimParser s ()
skipKeyword s = skipOptionalSpaces
    *> mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: [CharCode] -> TeXPrimParser s Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

parseKeywordToValue :: [CharCode] -> b -> TeXPrimParser s b
parseKeywordToValue s = (skipKeyword s $>)

parseManyChars :: TeXPrimParser s [CharCode]
parseManyChars = PC.many $ satisfyThen tokToChar
