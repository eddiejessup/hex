module Hex.Test.Lex where

import Data.Conduit
import Data.Conduit.Combinators
import Hex.Categorise
import Hex.Config.Codes
import Hex.Lex
import Protolude hiding (yield)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lex"
    [ testSimple,
      testControlWord,
      testControlSymbol,
      testControlSymbolThenLetters,
      testManySpaces,
      testComments
    ]

ccOrd :: Char -> CharCode
ccOrd = CharCode . wordOrd

wordOrd :: Char -> Word8
wordOrd = fromIntegral . ord

inCC :: Char -> CatCode -> RawCharCat
inCC c = RawCharCat (ccOrd c)

outCC :: Char -> CoreCatCode -> Token
outCC c cat = CharCatToken $ CharCat (ccOrd c) cat

assertResultsEqual :: Maybe LexState -> [RawCharCat] -> [Token] -> Assertion
assertResultsEqual mayLexState inp expected = do
  let testStates = case mayLexState of
        Nothing -> [LineBegin, LineMiddle, SkippingBlanks]
        Just lexState -> [lexState]
  for_ testStates $ \lexState -> do
    let res = evalState (runExceptT (run inp)) lexState
    case res of
      Left e ->
        assertFailure $ "With initial state '" <> show lexState <> "', got unexpected error: " <> show @(Identity LexError) e
      Right toks ->
        assertEqual ("With initial state '" <> show lexState <> "', Results match") expected toks
  where
    run :: [RawCharCat] -> ExceptT (Identity LexError) (StateT LexState Identity) [Token]
    run s =
      runConduit $
        yieldMany s
          .| extractToken
          .| sinkList

testSimple :: TestTree
testSimple =
  testCase "One letter maps to one letter" $
    assertResultsEqual
      Nothing
      [ inCC 'a' (CoreCatCode Letter)
      ]
      [ outCC 'a' Letter
      ]

testControlWord :: TestTree
testControlWord =
  testCase "'\\aa' maps to one control sequence" $
    assertResultsEqual
      Nothing
      [ inCC '\\' Escape,
        inCC 'a' (CoreCatCode Letter),
        inCC 'a' (CoreCatCode Letter)
      ]
      [ ControlSequenceToken $ ControlSequence "aa"
      ]

testControlSymbol :: TestTree
testControlSymbol =
  testCase "'\\.' maps to one control sequence" $
    assertResultsEqual
      Nothing
      [ inCC '\\' Escape,
        inCC '.' (CoreCatCode Other)
      ]
      [ ControlSequenceToken $ ControlSequence "."
      ]

testControlSymbolThenLetters :: TestTree
testControlSymbolThenLetters =
  testCase "'\\.hi' maps to one control sequence, then some letters" $
    assertResultsEqual
      Nothing
      [ inCC '\\' Escape,
        inCC '.' (CoreCatCode Other),
        inCC 'h' (CoreCatCode Letter),
        inCC 'i' (CoreCatCode Letter)
      ]
      [ ControlSequenceToken $ ControlSequence ".",
        outCC 'h' Letter,
        outCC 'i' Letter
      ]

testManySpaces :: TestTree
testManySpaces =
  testCase "Multiple spaces map to a single space" $ do
    assertResultsEqual
      (Just LineBegin)
      [ inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space)
      ]
      []

    assertResultsEqual
      (Just SkippingBlanks)
      [ inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space)
      ]
      []

    assertResultsEqual
      (Just LineMiddle)
      [ inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space)
      ]
      [ outCC ' ' Space
      ]

testComments :: TestTree
testComments =
  testCase "Comments are ignored" $ do
    assertResultsEqual
      Nothing
      [ inCC '#' Comment,
        inCC 'a' (CoreCatCode Letter),
        inCC '\n' EndOfLine,
        inCC 'b' (CoreCatCode Letter)
      ]
      [ parToken,
        outCC 'b' Letter
      ]
