module Hex.Lex where

import Data.Aeson (Value (..))
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Combinators (dropWhile, peek, sinkList)
import qualified Hex.Categorise as Cat
import qualified Hex.Config.Codes as Code
import Hexlude hiding (dropWhile)
import qualified Test.QuickCheck as QC

-- TODO: Fix this hashing crap.
newtype ControlSequence = ControlSequence ByteString
  deriving newtype (Show, Eq, Hashable)

instance ToJSON ControlSequence where
  toJSON (ControlSequence bs) = String $ "\\" <> decodeUtf8 bs

mkControlSequence :: [Code.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ Code.codeWord <$> csChars

instance Describe ControlSequence where
  describe (ControlSequence bs) =
    let s = "\\" <> decodeUtf8 bs
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

data CharCat = CharCat
  { char :: Code.CharCode,
    cat :: Code.CoreCatCode
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

parToken :: Token
parToken = ControlSequenceToken $ mkControlSequence $ Code.unsafeCodeFromChar <$> ("par" :: [Char])

data LexError
  = TrailingEscape
  deriving stock (Show, Generic)

extractToken ::
  forall m e.
  (MonadState LexState m, MonadError e m, AsType LexError e) =>
  ConduitT Cat.RawCharCat Token m ()
extractToken =
  await >>= \case
    Nothing -> pure ()
    Just (Cat.RawCharCat n1 cat1) -> do
      lexState <- get
      case (cat1, lexState) of
        -- Control sequence: Grab it.
        (Code.Escape, _) ->
          -- Get the next token.
          await >>= \case
            -- It is an error to see a single escape character at the end of the input.
            Nothing ->
              lift $ throwError $ injectTyped TrailingEscape
            Just csCC1@(Cat.RawCharCat _ ctrlSeqCat1) -> do
              ctrlSeqCCs <- case ctrlSeqCat1 of
                -- If the next token's category is 'Letter', keep taking tokens
                -- while they are also of category 'Letter'. Prepend the first
                -- letter to form our full control sequence.
                Code.CoreCatCode Code.Letter -> do
                  ctrlWordCCsPostFirst <- takeWhileLetter .| sinkList
                  pure (csCC1 : ctrlWordCCsPostFirst)
                -- If the next token is of a non-Letter category, the control
                -- sequence contains that single token.
                _ ->
                  pure (csCC1 : [])
              put $ case ctrlSeqCat1 of
                Code.CoreCatCode Code.Space -> SkippingBlanks
                Code.CoreCatCode Code.Letter -> SkippingBlanks
                _ -> LineMiddle
              yield $ ControlSequenceToken $ mkControlSequence $ Cat.char <$> ctrlSeqCCs
        -- Comment: Ignore rest of line and switch to line-begin.
        (Code.Comment, _) -> do
          dropWhile (\(Cat.RawCharCat _ cat) -> cat /= Code.EndOfLine)
          put LineBegin
        -- Empty line: Make a paragraph.
        (Code.EndOfLine, LineBegin) -> do
          yield parToken
          put LineBegin
        -- End of line in middle of line: Make a space token and go to line begin.
        (Code.EndOfLine, LineMiddle) -> do
          yield spaceTok
          put LineBegin
        -- Space in middle of line: Make a space token and start skipping blanks.
        (Code.CoreCatCode Code.Space, LineMiddle) -> do
          yield spaceTok
          put SkippingBlanks
        -- Space at the start of a line: Ignore.
        (Code.CoreCatCode Code.Space, LineBegin) ->
          pure ()
        -- Space or end of line, while skipping blanks: Ignore.
        (Code.CoreCatCode Code.Space, SkippingBlanks) ->
          pure ()
        (Code.EndOfLine, SkippingBlanks) ->
          pure ()
        -- Ignored: Ignore.
        (Code.Ignored, _) ->
          pure ()
        -- Invalid: Print error message and ignore.
        -- TODO: TeXbook says to print an error message in this case.
        (Code.Invalid, _) ->
          pure ()
        -- Simple tokeniser cases.
        (Code.CoreCatCode cc, _) -> do
          yield $ CharCatToken $ CharCat n1 cc
          put LineMiddle
      extractToken
  where
    takeWhileLetter :: ConduitT Cat.RawCharCat Cat.RawCharCat m ()
    takeWhileLetter =
      peek >>= \case
        Nothing -> pure ()
        Just cc@(Cat.RawCharCat _ (Code.CoreCatCode Code.Letter)) -> do
          -- Take what we just peeked.
          ccAgain <- await
          when (ccAgain /= Just cc) $ panic "Impossible"
          yield cc
          takeWhileLetter
        Just _ ->
          pure ()
