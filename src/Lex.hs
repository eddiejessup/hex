{-# LANGUAGE DuplicateRecordFields #-}

module Lex where

import qualified Data.Char as C

import qualified Cat

data ControlSequence
  = ControlSymbol Char
  | ControlWord String
  deriving (Show)

-- Not all Catcodes make it past the lexer, which we can represent in the
-- type system.
data LexCatCode
  = BeginGroup
  | EndGroup
  | MathShift
  | AlignTab
  | Parameter
  | Superscript
  | Subscript
  | Space
  | Letter
  | Other
  | Active
  deriving (Show)

data Token
  = CharCat {char :: Cat.CharCode, cat :: LexCatCode}
  | ControlSequence ControlSequence
  deriving (Show)

data LexState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving (Eq)

spaceTok :: Token
-- 32 means ' '.
spaceTok = CharCat{char=32, cat=Space}

extractToken :: LexState -> [Cat.CharCat] -> ([Token], LexState, [Cat.CharCat])
extractToken state [] = ([], state, [])
-- Control sequence: Grab it.
extractToken _ (Cat.CharCat{cat=Cat.Escape}:rest) =
    let
        ccNameFirst:remainCharCatsFirst = rest
        catNameFirst = Cat.cat ccNameFirst
        (cs, remainCharCats) = if catNameFirst == Cat.Letter
            then let
                (ccNameRest, remainCharCatsWord) = break ((Cat.Letter /=) . Cat.cat) remainCharCatsFirst
                cwName = fmap (C.chr . Cat.char) (ccNameFirst:ccNameRest)
            in (ControlWord cwName, remainCharCatsWord)
            else (ControlSymbol $ (C.chr . Cat.char) ccNameFirst, remainCharCatsFirst)
        nextState = if catNameFirst `elem` [Cat.Space, Cat.Letter] then SkippingBlanks else LineMiddle
    in
        ([ControlSequence cs], nextState, remainCharCats)
-- Comment: Ignore rest of line.
extractToken _ (Cat.CharCat{cat=Cat.Comment}:rest) =
    ([], LineBegin, dropWhile ((Cat.EndOfLine /=) . Cat.cat) rest)
-- Space at the start of a line: Ignore.
extractToken LineBegin (Cat.CharCat{cat=Cat.Space}:rest) =
    ([], LineBegin, rest)
-- Empty line: Make a paragraph.
extractToken LineBegin (Cat.CharCat{cat=Cat.EndOfLine}:rest) =
    ([ControlSequence $ ControlWord "par"], LineBegin, rest)
-- Simple tokeniser cases
extractToken _ (Cat.CharCat{char=n, cat=Cat.BeginGroup}:rest)
    = ([CharCat{char=n, cat=BeginGroup}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.EndGroup}:rest)
    = ([CharCat{char=n, cat=EndGroup}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.MathShift}:rest)
    = ([CharCat{char=n, cat=MathShift}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.AlignTab}:rest)
    = ([CharCat{char=n, cat=AlignTab}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.Parameter}:rest)
    = ([CharCat{char=n, cat=Parameter}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.Superscript}:rest)
    = ([CharCat{char=n, cat=Superscript}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.Subscript}:rest)
    = ([CharCat{char=n, cat=Subscript}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.Letter}:rest)
    = ([CharCat{char=n, cat=Letter}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.Other}:rest)
    = ([CharCat{char=n, cat=Other}], LineMiddle, rest)
extractToken _ (Cat.CharCat{char=n, cat=Cat.Active}:rest)
    = ([CharCat{char=n, cat=Active}], LineMiddle, rest)
-- Space, or end of line, while skipping blanks: Ignore.
extractToken SkippingBlanks (Cat.CharCat{cat=Cat.Space}:rest)
    = ([], SkippingBlanks, rest)
extractToken SkippingBlanks (Cat.CharCat{cat=Cat.EndOfLine}:rest)
    = ([], SkippingBlanks, rest)
-- Space in middle of line: Make a space token and start skipping blanks.
extractToken LineMiddle (Cat.CharCat{cat=Cat.Space}:rest)
    = ([spaceTok], SkippingBlanks, rest)
-- End of line in middle of line: Make a space token and go to line begin.
extractToken LineMiddle (Cat.CharCat{cat=Cat.EndOfLine}:rest)
    = ([spaceTok], LineBegin, rest)
-- Ignored: Ignore.
extractToken state (Cat.CharCat{cat=Cat.Ignored}:rest)
    = ([], state, rest)
-- Invalid: Print error message and ignore.
-- TODO: TeXbook says to print an error message in this case.
extractToken state (Cat.CharCat{cat=Cat.Invalid}:rest)
    = ([], state, rest)

extractAll :: LexState -> [Cat.CharCat] -> [Token]
extractAll _ [] = []
extractAll s ccs =
    let (thisToks, nextS, nextCCs) = extractToken s ccs
    in thisToks ++ extractAll nextS nextCCs

extractAllInit :: [Cat.CharCat] -> [Token]
extractAllInit = extractAll LineBegin
