{-# LANGUAGE StrictData #-}
module Hex.Lex where

import qualified Data.ByteString.Lazy as BS.L
import qualified Data.ByteString as BS
import qualified Hex.Categorise as Cat
import qualified Hex.Config.Codes as Code
import Hexlude
import qualified Test.QuickCheck as QC

-- TODO: Fix this hashing crap.
newtype ControlSequence = ControlSequence ByteString
  deriving newtype (Show, Eq, Hashable)

instance ToJSON ControlSequence where
  toJSON (ControlSequence bs) = String $ "\\" <> toS bs

mkControlSequence :: [Code.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ Code.codeWord <$> csChars

instance Describe ControlSequence where

  describe (ControlSequence bs) =
    let s = "\\" <> toS bs
    in singleLine $ "ControlSequence " <> quote s

-- Make sure code-list is non-empty.
instance Arbitrary ControlSequence where
  arbitrary =
    mkControlSequence <$> QC.listOf1 arbitrary

data ControlSequenceLike
  = ActiveCharacter Code.CharCode
  | ControlSequenceProper ControlSequence
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Hashable ControlSequenceLike

instance Describe ControlSequenceLike where

  describe = \case
    ControlSequenceProper cs ->
      describePrepended 0 "CSLike/ControlSequenceProper" cs
    ActiveCharacter cc ->
      describePrepended 0 "CSLike/ActiveCharacter" cc

instance Arbitrary ControlSequenceLike where
  arbitrary = genericArbitraryU

data CharCat
  = CharCat
      { char :: Code.CharCode
      , cat :: Code.CoreCatCode
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Describe CharCat where

  describe CharCat {char, cat} =
    [ (0, "CharCat")
    ]
    <> describeRel 1 cat
    <> describeRel 1 char

instance Arbitrary CharCat where
  arbitrary = genericArbitraryU

data Token
  = CharCatToken CharCat
  | ControlSequenceToken ControlSequence
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Describe Token where

  describe (CharCatToken cc) = describe cc
  describe (ControlSequenceToken cs) = describe cs

instance Arbitrary Token where
  arbitrary = genericArbitraryU

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
parToken = ControlSequenceToken $ mkControlSequence $ Code.unsafeCodeFromChar <$> ("par" :: [Char])

extractToken
  :: (Code.CharCode -> Code.CatCode)
  -> LexState
  -> BS.L.ByteString
  -> Maybe (Token, LexState, BS.L.ByteString)
extractToken charToCat = go
  where
    go :: LexState -> BS.L.ByteString -> Maybe (Token, LexState, BS.L.ByteString)
    go _state cs = do
      (Cat.RawCharCat n1 cat1, rest) <- getCC cs
      case (cat1, _state) of
        -- Control sequence: Grab it.
        (Code.Escape, _) -> do
          (csCC1@(Cat.RawCharCat _ ctrlSeqCat1), restPostEscape) <- getCC rest
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
            ( ControlSequenceToken $ mkControlSequence $ toList (Cat.char <$> ctrlSeqCCs)
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
        r@(Cat.RawCharCat _ (Code.CoreCatCode Code.Letter), _) ->
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
