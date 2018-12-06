{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Lexed.Inhibited where

import qualified Data.Char                     as C
import           Text.Megaparsec                ( (<|>) )

import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Lexed.Stream

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

newtype BalancedText =
  BalancedText [Lex.Token]
  deriving (Show, Eq)

isCategory :: Lex.LexCatCode -> Lex.Token -> Bool
isCategory a (Lex.CharCatToken Lex.CharCat{cat = b}) = a == b
isCategory _ _ = False

parseBalancedText :: SimpLexParser BalancedText
parseBalancedText = BalancedText <$> parseNestedBraces 1
  where
    parseNestedBraces :: Int -> SimpLexParser [Lex.Token]
    parseNestedBraces 0 = pure []
    parseNestedBraces n = do
      (x, nextN) <- satisfyThen parseNext
      case nextN of
        0 -> pure []
        _ -> (x :) <$> parseNestedBraces nextN
      where
        parseNext x@(Lex.CharCatToken Lex.CharCat {cat = Lex.BeginGroup}) =
          Just (x, succ n)
        parseNext x@(Lex.CharCatToken Lex.CharCat {cat = Lex.EndGroup}) =
          Just (x, pred n)
        parseNext x = Just (x, n)

-- Case 10.
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
    endsDelim t = (isCategory Lex.Parameter t) || (isCategory Lex.BeginGroup t)

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
  deriving (Eq, Ord, Bounded, Enum)

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

maybeParseParametersFrom :: Digit -> SimpLexParser [[Lex.Token]]
maybeParseParametersFrom dig = parseEndOfParams <|> parseParametersFrom
  where
    -- Parse the left-brace that indicates the end of parameters.
    parseEndOfParams = skipSatisfied (isCategory Lex.BeginGroup) *> pure []

    -- Parse a present parameter, then the remaining parameters, if present.
    parseParametersFrom = do
      -- Parse, for example, '#3'.
      skipSatisfied $ isCategory Lex.Parameter
      skipSatisfied matchesDigit
      -- Parse delimiter tokens after the parameter number, if present.
      thisParam <- parseParamDelims
      -- Return this parameter, plus any remaining parameters.
      (thisParam:) <$> case dig of
        -- If we are parsing parameter nine, there can't be any more, so we
        -- only expect to end the parameters.
        Nine -> parseEndOfParams
        -- Otherwise, we can either end the parameters, or have some more,
        -- starting from the successor of this digit.
        _ -> maybeParseParametersFrom (succ dig)

    matchesDigit (Lex.CharCatToken Lex.CharCat {char=chr, cat=cat})
      | isLetterOrOther cat = chr == digitToChar dig
      | otherwise = False
      where
        isLetterOrOther Lex.Letter = True
        isLetterOrOther Lex.Other = True
        isLetterOrOther _ = False
    matchesDigit _ = False

parseParamText :: SimpLexParser ([Lex.Token], [[Lex.Token]])
parseParamText = do
  preParamToks <- parseParamDelims
  parameters <- maybeParseParametersFrom minBound
  pure (preParamToks, parameters)
