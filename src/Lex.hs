{-# LANGUAGE DuplicateRecordFields #-}

module Lex where

import qualified Data.Char as C
import Data.Hashable (Hashable, hashWithSalt)
import qualified Categorise as Cat
import Categorise (CharCode)

data ControlSequence
  = ControlSymbol Char
  | ControlWord String
  deriving (Show, Eq)

data ControlSequenceLike = ActiveCharacter CharCode | ControlSequenceProper ControlSequence
  deriving (Show, Eq)

instance Hashable ControlSequenceLike where
  hashWithSalt s (ControlSequenceProper cs) = hashWithSalt s cs
  hashWithSalt s (ActiveCharacter cc) = hashWithSalt s cc

instance Hashable ControlSequence where
  hashWithSalt s (ControlSymbol c) = hashWithSalt s c
  hashWithSalt s (ControlWord w) = hashWithSalt s w

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
  deriving (Show, Eq)

data CharCat = CharCat {char :: CharCode, cat :: LexCatCode}
  deriving (Show, Eq)

data Token
  = CharCatToken CharCat
  | ControlSequence ControlSequence
  deriving (Show, Eq)

instance Ord Token where
  compare _ _ = EQ

data LexState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving (Eq, Show)

spaceTok :: Token
-- 32 means ' '.
spaceTok = CharCatToken CharCat{char=32, cat=Space}

chopDropWhile :: ([b] -> Maybe (a, [b])) -> (a -> Bool) -> [b] -> [b]
chopDropWhile _ _ [] = []
chopDropWhile getNext test cs =
  case getNext cs of
    Nothing -> []
    Just (next, rest) ->
      if test next then chopDropWhile getNext test rest else rest

chopBreakInner :: [a] -> ([b] -> Maybe (a, [b])) -> (a -> Bool) -> [b] -> ([a], [b])
chopBreakInner acc _ _ [] = (acc, [])
chopBreakInner acc getNext test cs =
  case getNext cs of
    Nothing -> (acc, [])
    Just (next, rest) ->
      if test next then (acc, cs) else chopBreakInner (next:acc) getNext test rest

-- Given a function that chops one 'a' from [b], and a stopping criterion on
-- 'a', get [a] up to but not including the succeeding item, and the rest of
-- the list, including the elements that generated the stopping 'a'.
chopBreak :: ([b] -> Maybe (a, [b])) -> (a -> Bool) -> [b] -> ([a], [b])
chopBreak get test cs = revFirst $ chopBreakInner [] get test cs
  where revFirst (a, b) = (reverse a, b)

isLetter :: Cat.CatCode -> Bool
isLetter Cat.Letter = True
isLetter _ = False

isEndOfLine :: Cat.CatCode -> Bool
isEndOfLine Cat.EndOfLine = True
isEndOfLine _ = False

extractToken :: ([CharCode] -> Maybe (Cat.CharCat, [CharCode])) -> LexState -> [CharCode] -> Maybe (Token, LexState, [CharCode])
extractToken _ _ [] = Nothing
extractToken getCC state cs = do
  (cc1, rest) <- getCC cs
  extractTokenRest cc1 rest
  where
    extractTokenRest Cat.CharCat{cat=cat1, char=n} rest
      -- Control sequence: Grab it.
      | Cat.Escape <- cat1 = do
          (cc2@Cat.CharCat{cat=cat2}, rest') <- getCC rest
          let
              (contSeq, rest2) = if isLetter cat2
                  then
                      let
                          (ccsNameRest, rest'') = chopBreak getCC (not . isLetter . Cat.cat) rest'
                          cwName = fmap (C.chr . Cat.char) (cc2:ccsNameRest)
                      in
                          (ControlWord cwName, rest'')
                  else
                      (ControlSymbol $ (C.chr . Cat.char) cc2, rest')
              nextState = case cat2 of
                  Cat.Space -> SkippingBlanks
                  Cat.Letter -> SkippingBlanks
                  _ -> LineMiddle
          return (ControlSequence contSeq, nextState, rest2)
      -- Comment: Ignore rest' of line and switch to line-begin.
      | Cat.Comment <- cat1 =
          extractToken getCC LineBegin $ chopDropWhile getCC (not . isEndOfLine . Cat.cat) rest
      -- Space at the start of a line: Ignore.
      | LineBegin <- state, Cat.Space <- cat1 =
          extractToken getCC state rest
      -- Empty line: Make a paragraph.
      | LineBegin <- state, Cat.EndOfLine <- cat1 =
          Just (ControlSequence $ ControlWord "par", LineBegin, rest)
      -- Simple tokeniser cases
      | Cat.BeginGroup <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=BeginGroup}, LineMiddle, rest)
      | Cat.EndGroup <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=EndGroup}, LineMiddle, rest)
      | Cat.MathShift <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=MathShift}, LineMiddle, rest)
      | Cat.AlignTab <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=AlignTab}, LineMiddle, rest)
      | Cat.Parameter <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=Parameter}, LineMiddle, rest)
      | Cat.Superscript <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=Superscript}, LineMiddle, rest)
      | Cat.Subscript <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=Subscript}, LineMiddle, rest)
      | Cat.Letter <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=Letter}, LineMiddle, rest)
      | Cat.Other <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=Other}, LineMiddle, rest)
      | Cat.Active <- cat1 =
          Just (CharCatToken CharCat{char=n, cat=Active}, LineMiddle, rest)
      -- Space, or end of line, while skipping blanks: Ignore.
      | SkippingBlanks <- state, Cat.Space <- cat1 =
          extractToken getCC state rest
      | SkippingBlanks <- state, Cat.EndOfLine <- cat1 =
          extractToken getCC state rest
      -- Space in middle of line: Make a space token and start skipping blanks.
      | LineMiddle <- state, Cat.Space <- cat1 =
          Just (spaceTok, SkippingBlanks, rest)
      -- End of line in middle of line: Make a space token and go to line begin.
      | LineMiddle <- state, Cat.EndOfLine <- cat1 =
          Just (spaceTok, LineBegin, rest)
      -- Ignored: Ignore.
      | Cat.Ignored <- cat1 =
          extractToken getCC state rest
      -- Invalid: Print error message and ignore.
      -- TODO: TeXbook says to print an error message in this case.
      | Cat.Invalid <- cat1 =
          extractToken getCC state rest
