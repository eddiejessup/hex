module Hex.Test.Categorise where

import Data.Conduit
import Data.Conduit.Combinators
import Hex.Categorise
import Hex.Config.Codes
import Protolude hiding (yield)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Categorise"
    [ testSimple,
      testTrioDown,
      testTrioUp,
      testTwoSuper,
      testOneSuper,
      testOneSuperLetter,
      testTwoDiffSupers,
      testSupersWithNewLine
    ]

ccOrd :: Char -> CharCode
ccOrd = CharCode . wordOrd

wordOrd :: Char -> Word8
wordOrd = fromIntegral . ord

ccChr :: CharCode -> Char
ccChr = chr . fromIntegral . codeWord

assertResultsEqual :: ByteString -> [RawCharCat] -> Assertion
assertResultsEqual inp expected =
  assertEqual "Results match" expected (run inp)
  where
    run :: ByteString -> [RawCharCat]
    run s =
      runConduitPure $
        yield s
          .| extractCharCat (charToCat . ccChr)
          .| sinkList

    charToCat :: Char -> CatCode
    charToCat = \case
      'a' -> CoreCatCode Letter
      '^' -> CoreCatCode Superscript
      '*' -> CoreCatCode Superscript
      '\n' -> EndOfLine
      _ -> Invalid

testSimple :: TestTree
testSimple =
  testCase "Single normal character translates simply" $
    assertResultsEqual
      "a"
      [ RawCharCat (ccOrd 'a') (CoreCatCode Letter)
      ]

testTrioDown :: TestTree
testTrioDown =
  testCase "Simple downward trio case translates to one correct char-cat" $
    assertResultsEqual
      "^^a"
      [ RawCharCat (CharCode (wordOrd 'a' - 64)) Invalid
      ]

testTrioUp :: TestTree
testTrioUp =
  testCase "Simple upward trio case translates to one correct char-cat" $
    assertResultsEqual
      "^^!"
      [ RawCharCat (CharCode (wordOrd '!' + 64)) (CoreCatCode Letter)
      ]

testTwoSuper :: TestTree
testTwoSuper =
  testCase "Two superscripts alone map to two usual char-cats" $
    assertResultsEqual
      "^^"
      [ RawCharCat (ccOrd '^') (CoreCatCode Superscript),
        RawCharCat (ccOrd '^') (CoreCatCode Superscript)
      ]

testOneSuper :: TestTree
testOneSuper =
  testCase "One superscript alone maps to one usual char-cat" $
    assertResultsEqual
      "^"
      [ RawCharCat (ccOrd '^') (CoreCatCode Superscript)
      ]

testOneSuperLetter :: TestTree
testOneSuperLetter =
  testCase "Single superscript and letter map to usual char-cats" $
    assertResultsEqual
      "^a"
      [ RawCharCat (ccOrd '^') (CoreCatCode Superscript),
        RawCharCat (ccOrd 'a') (CoreCatCode Letter)
      ]

testTwoDiffSupers :: TestTree
testTwoDiffSupers =
  testCase "Two different superscripts and letter don't cause a trio" $
    assertResultsEqual
      "^*a"
      [ RawCharCat (ccOrd '^') (CoreCatCode Superscript),
        RawCharCat (ccOrd '*') (CoreCatCode Superscript),
        RawCharCat (ccOrd 'a') (CoreCatCode Letter)
      ]

testSupersWithNewLine :: TestTree
testSupersWithNewLine =
  testCase "Two superscripts then a end-of-line don't cause a trio" $
    assertResultsEqual
      "^^\n"
      [ RawCharCat (ccOrd '^') (CoreCatCode Superscript),
        RawCharCat (ccOrd '^') (CoreCatCode Superscript),
        RawCharCat (ccOrd '\n') EndOfLine
      ]
