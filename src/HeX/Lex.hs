{-# LANGUAGE DeriveAnyClass #-}

module HeX.Lex where

import           HeXlude

import           Data.Hashable  (Hashable)
import qualified Data.Sequence as Seq
import           HeX.Categorise (CatCode, CharCode)
import qualified HeX.Categorise as Cat

newtype ControlSequence = ControlSequence (Seq CharCode)
    deriving stock (Show, Eq, Generic)
    deriving newtype (Hashable)

instance Readable ControlSequence where
    describe (ControlSequence ccs) = "\\" <> toS ccs

data ControlSequenceLike
    = ActiveCharacter CharCode
    | ControlSequenceProper ControlSequence
    deriving (Show, Eq, Generic, Hashable)

data CharCat = CharCat
    { char :: CharCode
    , cat  :: Cat.CoreCatCode
    } deriving (Show, Eq)

instance Readable CharCat where
    describe CharCat { char, cat } = toS [char] <> "[" <> describe cat <> "]"

data Token
    = CharCatToken CharCat
    | ControlSequenceToken ControlSequence
    deriving (Show, Eq)

instance Readable Token where
    describe (CharCatToken cc) = describe cc
    describe (ControlSequenceToken cs) = describe cs

instance Ord Token where
    compare _ _ = EQ

data LexState
    = SkippingBlanks
    | LineMiddle
    | LineBegin
    deriving (Eq, Show)

spaceTok :: Token
spaceTok = CharCatToken $ CharCat ' ' Cat.Space

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
chopBreak
    :: (b -> Maybe (a, b))
    -> b
    -> (Seq a, b)
chopBreak getNext xs = go Empty xs
  where
    go acc cs = case getNext cs of
        Nothing ->
            (acc, cs)
        Just (next, csRest) ->
            go (acc :|> next) csRest

parToken :: Token
parToken = ControlSequenceToken $ ControlSequence ("par")

extractToken
    :: (CharCode -> CatCode)
    -> LexState
    -> Seq CharCode
    -> Maybe (Token, LexState, Seq CharCode)
extractToken _     _     Empty = Nothing
extractToken charToCat _state cs =
    do
    (cc1, rest) <- getCC cs
    extractTokenRest cc1 rest
  where
    getCC = Cat.extractCharCat charToCat

    getLetterCC xs =
        getCC xs >>= \case
            r@(Cat.CharCat _ (Cat.CoreCatCode Cat.Letter), _) ->
                pure r
            _ ->
                Nothing

    extractTokenRest (Cat.CharCat n cat1) rest = case (cat1, _state) of
        -- Control sequence: Grab it.
        (Cat.Escape, _) ->
            do
            (csCC1@(Cat.CharCat _ ctrlSeqCat1), restPostEscape) <- getCC rest
            let (ctrlSeqCCs, restPostCtrlSeq) = case ctrlSeqCat1 of
                    Cat.CoreCatCode Cat.Letter ->
                        let (ctrlWordCCsPostFirst, restPostCtrlWord) = chopBreak getLetterCC restPostEscape
                        in  ((csCC1 :<| ctrlWordCCsPostFirst), restPostCtrlWord)
                    _ ->
                        (singleton csCC1, restPostEscape)
                nextState = case ctrlSeqCat1 of
                    Cat.CoreCatCode Cat.Space  -> SkippingBlanks
                    Cat.CoreCatCode Cat.Letter -> SkippingBlanks
                    _                          -> LineMiddle
            pure ( ControlSequenceToken $ ControlSequence $ Cat.char <$> ctrlSeqCCs
                 , nextState
                 , restPostCtrlSeq
                 )
        -- Comment: Ignore rest of line and switch to line-begin.
        (Cat.Comment, _) ->
            extractToken charToCat LineBegin $
                Seq.dropWhileL (\c -> charToCat c /= Cat.EndOfLine) rest
        -- Space at the start of a line: Ignore.
        (Cat.CoreCatCode Cat.Space, LineBegin) ->
            extractToken charToCat _state rest
        -- Empty line: Make a paragraph.
        (Cat.EndOfLine, LineBegin) ->
            pure (parToken, LineBegin, rest)
        -- Space, or end of line, while skipping blanks: Ignore.
        (Cat.CoreCatCode Cat.Space, SkippingBlanks) ->
            extractToken charToCat _state rest
        (Cat.EndOfLine, SkippingBlanks) ->
            extractToken charToCat  _state rest
        -- Space in middle of line: Make a space token and start skipping blanks.
        (Cat.CoreCatCode Cat.Space, LineMiddle) ->
            pure (spaceTok, SkippingBlanks, rest)
        -- End of line in middle of line: Make a space token and go to line begin.
        (Cat.EndOfLine, LineMiddle) ->
            pure (spaceTok, LineBegin, rest)
        -- Ignored: Ignore.
        (Cat.Ignored, _) ->
            extractToken charToCat _state rest
        -- Invalid: Print error message and ignore.
        -- TODO: TeXbook says to print an error message in this case.
        (Cat.Invalid, _) ->
            extractToken charToCat _state rest
        -- Simple tokeniser cases.
        (Cat.CoreCatCode cc, _) ->
            pure (CharCatToken $ CharCat n cc, LineMiddle, rest)
