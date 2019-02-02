{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Lex where

import           Data.Hashable                  ( Hashable
                                                , hashWithSalt
                                                )

import qualified HeX.Categorise                as Cat
import           HeX.Categorise                 ( CharCode, CatCode )

newtype ControlSequence = ControlSequence String
    deriving (Show, Eq)

data ControlSequenceLike
    = ActiveCharacter CharCode
    | ControlSequenceProper ControlSequence
    deriving (Show, Eq)

instance Hashable ControlSequenceLike where
    hashWithSalt s (ControlSequenceProper cs) = hashWithSalt s cs
    hashWithSalt s (ActiveCharacter cc) = hashWithSalt s cc

instance Hashable ControlSequence where
    hashWithSalt s (ControlSequence w) = hashWithSalt s w

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

data CharCat = CharCat
    { char :: CharCode
    , cat  :: LexCatCode
    } deriving (Show, Eq)

data Token
    = CharCatToken CharCat
    | ControlSequenceToken ControlSequence
    deriving (Show, Eq)

instance Ord Token where
    compare _ _ = EQ

data LexState
    = SkippingBlanks
    | LineMiddle
    | LineBegin
    deriving (Eq, Show)

spaceTok :: Token
spaceTok = CharCatToken $ CharCat ' ' Space

chopDropWhile :: ([b] -> Maybe (a, [b])) -> (a -> Bool) -> [b] -> [b]
chopDropWhile _       _    [] = []
chopDropWhile getNext test cs = case getNext cs of
    Nothing         -> []
    Just (next, rest)
        | test next -> chopDropWhile getNext test rest
        | otherwise -> rest

-- Given a function that chops one 'a' from [b], and a stopping criterion on
-- 'a', get [a] up to but not including the succeeding item, and the rest of
-- the list, including the elements that generated the stopping 'a'.
chopBreak :: ([b] -> Maybe (a, [b])) -> (a -> Bool) -> [b] -> ([a], [b])
chopBreak get test cs = revFirst $ inner [] get cs
  where
    revFirst (a, b) = (reverse a, b)

    inner acc _       []  = (acc, [])
    inner acc getNext cs' = case getNext cs' of
        Nothing         -> (acc, [])
        Just (next, rest)
            | test next -> (acc, cs')
            | otherwise -> inner (next : acc) getNext rest

extractToken
    :: (CharCode -> CatCode)
    -> LexState
    -> [CharCode]
    -> Maybe (Token, LexState, [CharCode])
extractToken _     _     [] = Nothing
extractToken charToCat state cs =
    do
    (cc1, rest) <- getCC cs
    extractTokenRest cc1 rest
  where
    getCC = Cat.extractCharCat charToCat

    extractTokenRest (Cat.CharCat n cat1) rest = case (cat1, state) of
        -- Control sequence: Grab it.
        (Cat.Escape, _) ->
            do
            (cc2@(Cat.CharCat _ cat2), rest') <- getCC rest
            let (controlCCs, rest2) = if cat2 == Cat.Letter
                    then
                        let (cwNameCCsRest, rest'') =
                                chopBreak getCC ((/= Cat.Letter) . Cat.cat) rest'
                        in  (cc2 : cwNameCCsRest, rest'')
                    else
                        ([cc2], rest')
                nextState = case cat2 of
                    Cat.Space  -> SkippingBlanks
                    Cat.Letter -> SkippingBlanks
                    _          -> LineMiddle
            pure ( ControlSequenceToken $ ControlSequence $ fmap Cat.char controlCCs
                 , nextState
                 , rest2
                 )
        -- Comment: Ignore rest of line and switch to line-begin.
        (Cat.Comment, _) ->
            extractToken charToCat LineBegin $
                dropWhile (\c -> charToCat c /= Cat.EndOfLine) rest
        -- Space at the start of a line: Ignore.
        (Cat.Space, LineBegin) ->
            extractToken charToCat state rest
        -- Empty line: Make a paragraph.
        (Cat.EndOfLine, LineBegin) ->
            pure (ControlSequenceToken $ ControlSequence "par", LineBegin, rest)
        -- Simple tokeniser cases.
        (Cat.BeginGroup, _) ->
            pure (CharCatToken $ CharCat n BeginGroup, LineMiddle, rest)
        (Cat.EndGroup, _) ->
            pure (CharCatToken $ CharCat n EndGroup, LineMiddle, rest)
        (Cat.MathShift, _) ->
            pure (CharCatToken $ CharCat n MathShift, LineMiddle, rest)
        (Cat.AlignTab, _) ->
            pure (CharCatToken $ CharCat n AlignTab, LineMiddle, rest)
        (Cat.Parameter, _) ->
            pure (CharCatToken $ CharCat n Parameter, LineMiddle, rest)
        (Cat.Superscript, _) ->
            pure (CharCatToken $ CharCat n Superscript, LineMiddle, rest)
        (Cat.Subscript, _) ->
            pure (CharCatToken $ CharCat n Subscript, LineMiddle, rest)
        (Cat.Letter, _) ->
            pure (CharCatToken $ CharCat n Letter, LineMiddle, rest)
        (Cat.Other, _) ->
            pure (CharCatToken $ CharCat n Other, LineMiddle, rest)
        (Cat.Active, _) ->
            pure (CharCatToken $ CharCat n Active, LineMiddle, rest)
        -- Space, or end of line, while skipping blanks: Ignore.
        (Cat.Space, SkippingBlanks) ->
            extractToken charToCat state rest
        (Cat.EndOfLine, SkippingBlanks) ->
            extractToken charToCat  state rest
        -- Space in middle of line: Make a space token and start skipping blanks.
        (Cat.Space, LineMiddle) ->
            pure (spaceTok, SkippingBlanks, rest)
        -- End of line in middle of line: Make a space token and go to line begin.
        (Cat.EndOfLine, LineMiddle) ->
            pure (spaceTok, LineBegin, rest)
        -- Ignored: Ignore.
        (Cat.Ignored, _) ->
            extractToken charToCat state rest
        -- Invalid: Print error message and ignore.
        -- TODO: TeXbook says to print an error message in this case.
        (Cat.Invalid, _) ->
            extractToken charToCat state rest
