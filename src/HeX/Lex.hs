{-# LANGUAGE StrictData #-}
module Hex.Lex where

import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Sequence as Seq
import qualified Hex.Categorise as Cat
import qualified Hex.Config.Codes as Code
import Hexlude

data ControlSequence
  = ControlSequence
      { csChars :: Seq Code.CharCode
      , csHash :: Int
      }
  deriving stock (Show, Eq)

mkControlSequence :: Seq Code.CharCode -> ControlSequence
mkControlSequence csChars = ControlSequence {csChars, csHash = hash csChars}

instance Hashable ControlSequence where

  hashWithSalt salt ControlSequence {csHash} =
    salt + csHash

  hash ControlSequence {csHash} =
    csHash

instance Readable ControlSequence where

  describe ControlSequence {csChars} = "\\" <> toS (toList (Code.unsafeCodeAsChar <$> csChars))

data ControlSequenceLike
  = ActiveCharacter Code.CharCode
  | ControlSequenceProper ControlSequence
  deriving stock (Show, Eq)

instance Hashable ControlSequenceLike where

  hashWithSalt salt csLike =
    salt + case csLike of
      ActiveCharacter c ->
        Code.codeInt c
      ControlSequenceProper cs ->
        hashWithSalt salt cs

  hash = \case
    ActiveCharacter c ->
      Code.codeInt c
    ControlSequenceProper cs ->
      hash cs

data CharCat
  = CharCat
      { char :: Code.CharCode
      , cat :: Code.CoreCatCode
      }
  deriving stock (Show, Eq)

instance Readable CharCat where

  describe CharCat {char, cat} = describe char <> "[" <> describe cat <> "]"

data Token
  = CharCatToken CharCat
  | ControlSequenceToken ControlSequence
  deriving stock (Show, Eq)

instance Readable Token where

  describe (CharCatToken cc) = describe cc
  describe (ControlSequenceToken cs) = describe cs

instance Ord Token where

  compare _ _ = EQ

data LexState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving stock (Eq, Show)

spaceTok :: Token
spaceTok = CharCatToken $ CharCat (Code.CharCode_ ' ') Code.Space

chopDropWhile :: ([b] -> Maybe (a, [b])) -> (a -> Bool) -> [b] -> [b]
chopDropWhile _ _ [] = []
chopDropWhile getNext test cs = case getNext cs of
  Nothing -> []
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
chopBreak getNext = go Empty
  where
    go acc cs = case getNext cs of
      Nothing ->
        (acc, cs)
      Just (c, csRest) ->
        go (acc :|> c) csRest

parToken :: Token
parToken = ControlSequenceToken $ mkControlSequence $ Seq.fromList $ Code.unsafeCodeFromChar <$> ("par" :: [Char])

extractToken
  :: (Code.CharCode -> Code.CatCode)
  -> LexState
  -> BS.L.ByteString
  -> Maybe (Token, LexState, BS.L.ByteString)
extractToken charToCat = go
  where
    go :: LexState -> BS.L.ByteString -> Maybe (Token, LexState, BS.L.ByteString)
    go _state cs = do
      (Cat.CharCat n1 cat1, rest) <- getCC cs
      case (cat1, _state) of
        -- Control sequence: Grab it.
        (Code.Escape, _) -> do
          (csCC1@(Cat.CharCat _ ctrlSeqCat1), restPostEscape) <- getCC rest
          let (ctrlSeqCCs, restPostCtrlSeq) = case ctrlSeqCat1 of
                Code.CoreCatCode Code.Letter ->
                  let (ctrlWordCCsPostFirst, restPostCtrlWord) = chopBreak getLetterCC restPostEscape
                  in (csCC1 :<| ctrlWordCCsPostFirst, restPostCtrlWord)
                _ ->
                  (singleton csCC1, restPostEscape)
              nextState = case ctrlSeqCat1 of
                Code.CoreCatCode Code.Space -> SkippingBlanks
                Code.CoreCatCode Code.Letter -> SkippingBlanks
                _ -> LineMiddle
          pure
            ( ControlSequenceToken $ mkControlSequence (Cat.char <$> ctrlSeqCCs)
            , nextState
            , restPostCtrlSeq
            )
        -- Comment: Ignore rest of line and switch to line-begin.
        (Code.Comment, _) ->
          go LineBegin $ BS.L.dropWhile (\c -> charToCat (Code.CharCode c) /= Code.EndOfLine) rest
        -- Empty line: Make a paragraph.
        (Code.EndOfLine, LineBegin) ->
          pure (parToken, LineBegin, rest)
        -- End of line in middle of line: Make a space token and go to line begin.
        (Code.EndOfLine, LineMiddle) ->
          pure (spaceTok, LineBegin, rest)
        -- Space in middle of line: Make a space token and start skipping blanks.
        (Code.CoreCatCode Code.Space, LineMiddle) ->
          pure (spaceTok, SkippingBlanks, rest)
        -- Space at the start of a line: Ignore.
        (Code.CoreCatCode Code.Space, LineBegin) ->
          go _state rest
        -- Space or end of line, while skipping blanks: Ignore.
        (Code.CoreCatCode Code.Space, SkippingBlanks) ->
          go _state rest
        (Code.EndOfLine, SkippingBlanks) ->
          go _state rest
        -- Ignored: Ignore.
        (Code.Ignored, _) ->
          go _state rest
        -- Invalid: Print error message and ignore.
        -- TODO: TeXbook says to print an error message in this case.
        (Code.Invalid, _) ->
          go _state rest
        -- Simple tokeniser cases.
        (Code.CoreCatCode cc, _) ->
          pure (CharCatToken $ CharCat n1 cc, LineMiddle, rest)
    getCC = Cat.extractCharCat charToCat
    getLetterCC xs =
      getCC xs >>= \case
        r@(Cat.CharCat _ (Code.CoreCatCode Code.Letter), _) ->
          pure r
        _ ->
          Nothing

codesToLexTokens :: (Code.CharCode -> Code.CatCode) -> BS.L.ByteString -> [Hex.Lex.Token]
codesToLexTokens charToCat = go LineBegin
  where
    go lexState xs = case extractToken charToCat lexState xs of
      Just (tok, lexState1, xs1) ->
        tok : go lexState1 xs1
      Nothing ->
        []

usableCodesToLexTokens :: BS.L.ByteString -> [Hex.Lex.Token]
usableCodesToLexTokens = codesToLexTokens Code.usableCatLookup
