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

-- extractToken :: LexState -> [Cat.CharCode] -> ([Token], LexState, [Cat.CharCode])
-- extractToken state [] = ([], state, [])
-- -- Control sequence: Grab it.
-- extractToken _ (Cat.CharCat{cat=Cat.Escape}:rest) =
--     let
--         ccNameFirst:remainCharCatsFirst = rest
--         catNameFirst = Cat.cat ccNameFirst
--         (cs, remainCharCats) = if catNameFirst == Cat.Letter
--             then let
--                 (ccNameRest, remainCharCatsWord) = break ((Cat.Letter /=) . Cat.cat) remainCharCatsFirst
--                 cwName = fmap (C.chr . Cat.char) (ccNameFirst:ccNameRest)
--             in (ControlWord cwName, remainCharCatsWord)
--             else (ControlSymbol $ (C.chr . Cat.char) ccNameFirst, remainCharCatsFirst)
--         nextState = if catNameFirst `elem` [Cat.Space, Cat.Letter] then SkippingBlanks else LineMiddle
--     in
--         ([ControlSequence cs], nextState, remainCharCats)
-- -- Comment: Ignore rest of line.
-- extractToken _ (Cat.CharCat{cat=Cat.Comment}:rest) =
--     ([], LineBegin, dropWhile ((Cat.EndOfLine /=) . Cat.cat) rest)
-- -- Space at the start of a line: Ignore.
-- extractToken LineBegin (Cat.CharCat{cat=Cat.Space}:rest) =
--     ([], LineBegin, rest)
-- -- Empty line: Make a paragraph.
-- extractToken LineBegin (Cat.CharCat{cat=Cat.EndOfLine}:rest) =
--     ([ControlSequence $ ControlWord "par"], LineBegin, rest)
-- Simple tokeniser cases
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.BeginGroup}:rest)
--     = ([CharCat{char=n, cat=BeginGroup}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.EndGroup}:rest)
--     = ([CharCat{char=n, cat=EndGroup}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.MathShift}:rest)
--     = ([CharCat{char=n, cat=MathShift}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.AlignTab}:rest)
--     = ([CharCat{char=n, cat=AlignTab}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.Parameter}:rest)
--     = ([CharCat{char=n, cat=Parameter}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.Superscript}:rest)
--     = ([CharCat{char=n, cat=Superscript}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.Subscript}:rest)
--     = ([CharCat{char=n, cat=Subscript}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.Letter}:rest)
--     = ([CharCat{char=n, cat=Letter}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.Other}:rest)
--     = ([CharCat{char=n, cat=Other}], LineMiddle, rest)
-- extractToken _ (Cat.CharCat{char=n, cat=Cat.Active}:rest)
--     = ([CharCat{char=n, cat=Active}], LineMiddle, rest)
-- -- Space, or end of line, while skipping blanks: Ignore.
-- extractToken SkippingBlanks (Cat.CharCat{cat=Cat.Space}:rest)
--     = ([], SkippingBlanks, rest)
-- extractToken SkippingBlanks (Cat.CharCat{cat=Cat.EndOfLine}:rest)
--     = ([], SkippingBlanks, rest)
-- -- Space in middle of line: Make a space token and start skipping blanks.
-- extractToken LineMiddle (Cat.CharCat{cat=Cat.Space}:rest)
--     = ([spaceTok], SkippingBlanks, rest)
-- -- End of line in middle of line: Make a space token and go to line begin.
-- extractToken LineMiddle (Cat.CharCat{cat=Cat.EndOfLine}:rest)
--     = ([spaceTok], LineBegin, rest)
-- -- Ignored: Ignore.
-- extractToken state (Cat.CharCat{cat=Cat.Ignored}:rest)
--     = ([], state, rest)
-- Invalid: Print error message and ignore.
-- TODO: TeXbook says to print an error message in this case.
-- extractToken state (Cat.CharCat{cat=Cat.Invalid}:rest)
--     = ([], state, rest)

chopDropWhile :: ([b] -> (a, [b])) -> (a -> Bool) -> [b] -> [b]
chopDropWhile _ _ [] = []
chopDropWhile getCC test cs  =
    let
        (cc1, csRest) = getCC cs
    in
        if test cc1 then chopDropWhile getCC test csRest else csRest

chopBreakInner :: [a] -> ([b] -> (a, [b])) -> (a -> Bool) -> [b] -> ([a], [b])
chopBreakInner acc _ _ [] = (acc, [])
chopBreakInner acc getCC test cs  =
    let
        (cc1, csRest) = getCC cs
    in
        if test cc1 then (reverse acc, cs) else chopBreakInner (cc1:acc) getCC test csRest

-- Given a function that chops one 'a' from [b], and a stopping criterion on
-- 'a', get [a] up to but not including the succeeding item, and the rest of
-- the list, including the elements that generated the stopping 'a'.
chopBreak :: ([b] -> (a, [b])) -> (a -> Bool) -> [b] -> ([a], [b])
chopBreak = chopBreakInner []

isLetter :: Cat.CatCode -> Bool
isLetter Cat.Letter = True
isLetter _ = False

isEndOfLine :: Cat.CatCode -> Bool
isEndOfLine Cat.EndOfLine = True
isEndOfLine _ = False

extractToken :: Cat.CharCatMap -> LexState -> [Cat.CharCode] -> (Token, LexState, [Cat.CharCode])
extractToken ccMap state cs
    -- Control sequence: Grab it.
    | Cat.Escape <- cat1 =
        let
            (cc2, rest2) = getCC rest
            cat2 = Cat.cat cc2
            (contSeq, rest3) = if isLetter cat2
                then
                    let
                        (ccsNameRest, rest3) = chopBreak getCC (not . isLetter . Cat.cat) rest2
                        cwName = fmap (C.chr . Cat.char) (cc2:ccsNameRest)
                    in
                        (ControlWord cwName, rest3)
                else
                    (ControlSymbol $ (C.chr . Cat.char) cc2, rest2)
            nextState = case cat2 of
                Cat.Space -> SkippingBlanks
                Cat.Letter -> SkippingBlanks
                _ -> LineMiddle
        in
            (ControlSequence contSeq, nextState, rest3)
    -- Comment: Ignore rest of line and switch to line-begin.
    | Cat.Comment <- cat1 =
        extractToken ccMap LineBegin $ chopDropWhile getCC (not . isEndOfLine . Cat.cat) rest
    -- Space at the start of a line: Ignore.
    | LineBegin <- state, Cat.Space <- cat1 =
        extractToken ccMap state rest
    -- Empty line: Make a paragraph.
    | LineBegin <- state, Cat.EndOfLine <- cat1 =
        (ControlSequence $ ControlWord "par", LineBegin, rest)
    -- Simple tokeniser cases
    | Cat.BeginGroup <- cat1 =
        (CharCat{char=n, cat=BeginGroup}, LineMiddle, rest)
    | Cat.EndGroup <- cat1 =
        (CharCat{char=n, cat=EndGroup}, LineMiddle, rest)
    | Cat.MathShift <- cat1 =
        (CharCat{char=n, cat=MathShift}, LineMiddle, rest)
    | Cat.AlignTab <- cat1 =
        (CharCat{char=n, cat=AlignTab}, LineMiddle, rest)
    | Cat.Parameter <- cat1 =
        (CharCat{char=n, cat=Parameter}, LineMiddle, rest)
    | Cat.Superscript <- cat1 =
        (CharCat{char=n, cat=Superscript}, LineMiddle, rest)
    | Cat.Subscript <- cat1 =
        (CharCat{char=n, cat=Subscript}, LineMiddle, rest)
    | Cat.Letter <- cat1 =
        (CharCat{char=n, cat=Letter}, LineMiddle, rest)
    | Cat.Other <- cat1 =
        (CharCat{char=n, cat=Other}, LineMiddle, rest)
    | Cat.Active <- cat1 =
        (CharCat{char=n, cat=Active}, LineMiddle, rest)
    -- Space, or end of line, while skipping blanks: Ignore.
    | SkippingBlanks <- state, Cat.Space <- cat1 =
        extractToken ccMap state rest
    | SkippingBlanks <- state, Cat.EndOfLine <- cat1 =
        extractToken ccMap state rest
    -- Space in middle of line: Make a space token and start skipping blanks.
    | LineMiddle <- state, Cat.Space <- cat1 =
        (spaceTok, SkippingBlanks, rest)
    -- End of line in middle of line: Make a space token and go to line begin.
    | LineMiddle <- state, Cat.EndOfLine <- cat1 =
        (spaceTok, LineBegin, rest)
    -- Ignored: Ignore.
    | Cat.Ignored <- cat1 =
        extractToken ccMap state rest
    -- Invalid: Print error message and ignore.
    -- TODO: TeXbook says to print an error message in this case.
    | Cat.Invalid <- cat1 =
        extractToken ccMap state rest
    where
        getCC = Cat.extractCharCat ccMap
        (Cat.CharCat{cat=cat1, char=n}, rest) = getCC cs

extractAllInner :: Cat.CharCatMap -> LexState -> [Cat.CharCode] -> [Token]
extractAllInner _ _ [] = []
extractAllInner ccMap state cs =
    let (thisTok, nextState, rest) = extractToken ccMap state cs
    in thisTok:extractAllInner ccMap nextState rest

extractAll :: Cat.CharCatMap -> [Cat.CharCode] -> [Token]
extractAll ccMap = extractAllInner ccMap LineBegin
